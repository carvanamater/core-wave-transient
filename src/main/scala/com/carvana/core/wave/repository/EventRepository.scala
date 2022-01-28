package com.carvana.core.wave.repository

import java.sql.SQLException
import java.util.UUID

import com.carvana.core.CatchEmAllException
import com.carvana.core.wave.model.{Event, Metadata}
import com.carvana.core.wave.tapir.tracing.TracingUtil._
import io.getquill.Ord
import io.getquill.context.ZioJdbc.QIO
import io.getquill.context.qzio.ImplicitSyntax.{Implicit, ImplicitSyntaxOps}
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator
import javax.sql.DataSource
import zio.clock.Clock
import zio.duration._
import zio.macros.accessible
import zio.stream.ZStream
import zio.telemetry.opentelemetry.Tracing
import zio._

import scala.collection.mutable

@accessible[EventRepository.Service]
object EventRepository {

  val DefaultBatchSize: Int                 = 1024 * 4
  val DefaultDurationBetweenPolls: Duration = 1.second

  case class EventId(id: Long)

  val liveTraced: ZLayer[Has[EventRepository.Service] with Tracing, Nothing, Has[EventRepository.Service]] = {
    (for {
      delegate <- ZIO.service[EventRepository.Service]
      tracing  <- ZIO.service[Tracing.Service]
    } yield {
      new LiveDelegating(delegate) {
        private val provide = Has(tracing)

        override def createNoTxn(event: Event): QIO[Event] = {
          val context = mutable.Map[String, String]()

          for {
            _      <- Tracing
              .inject(W3CTraceContextPropagator.getInstance(), context, ProducerSetter)
              .provide(provide)
            event2  = event.copy(
              metadata = event.metadata.copy(
                annotations = event.metadata.annotations ++ context
              )
            )
            event3 <- delegate.createNoTxn(event2)
          } yield event3
        }
      }
    }).toLayer
  }

  val live: ZLayer[Has[DataSource] with Clock, Nothing, Has[EventRepository.Service]] = {
    (for {
      datasource <- ZIO.service[DataSource]
      clock      <- ZIO.service[Clock.Service]
    } yield Live(datasource, clock)).toLayer
  }

  object Queries extends EventQuerySupport {
    import ctx._

    lazy val events   = quote(querySchema[Event]("events"))
    lazy val eventIds = quote(querySchema[EventId]("events"))

    def getLastIdQ = quote(eventIds.map(_.id).sortBy(v => v)(Ord.desc).take(1))

    def getLastId = ctx.run(getLastIdQ)

    def getQ(id: UUID) = quote(events.filter(_.metadata.id == lift(id)).take(1))

    def get(id: UUID) = ctx.run(getQ(id)).flatMap(Queries.singleRow(_)(s"could not find event with id $id"))

    def deleteQ(id: UUID) = quote(events.filter(_.metadata.id == lift(id)).delete)

    def delete(id: UUID) = ctx.run(deleteQ(id))

    def createQ(event: Event) = {
      quote(events.insert(lift(event)).returningGenerated(o => (o.metadata.id, o.metadata.createTime, o.metadata.updateTime)))
    }

    def create(event: Event) = ctx.run(createQ(event)).map { case (id, createTime, updateTime) =>
      event.copy(
        metadata = event.metadata.copy(id = id, createTime = createTime, updateTime = updateTime)
      )
    }

    def streamQ(from: Long, take: Int) =
      quote(
        events
          .filter(t => infix"$t.id > ${lift(from)}".as[Boolean])
          .take(lift(take))
          .map(t => (t, infix"$t.id".as[Long]))
      )

    def stream(from: Long, take: Int) = ctx.run(streamQ(from, take))
  }

  trait Service extends Repository {
    def createNoTxn(event: Event): QuerySupport.DEFAULT_CONTEXT.Result[Event]
    def get(id: UUID): IO[CatchEmAllException, Event]
    def delete(id: UUID): IO[CatchEmAllException, Unit]

    def stream(
      from: Option[UUID] = None,
      batchSize: Int = DefaultBatchSize,
      durationBetweenPolls: Duration = DefaultDurationBetweenPolls
    ): IO[CatchEmAllException, ZStream[Any, CatchEmAllException, Event]]
  }

  class LiveDelegating(delegate: EventRepository.Service) extends Service {
    override def createNoTxn(event: Event): QIO[Event] = delegate.createNoTxn(event)

    override def get(id: UUID): IO[CatchEmAllException, Event] = delegate.get(id)

    override def delete(id: UUID): IO[CatchEmAllException, Unit] = delegate.delete(id)

    override def stream(
      from: Option[UUID],
      batchSize: Int,
      durationBetweenPolls: Duration
    ): IO[CatchEmAllException, ZStream[Any, CatchEmAllException, Event]] =
      delegate.stream(from, batchSize, durationBetweenPolls)

    override def metadataRepository: MetadataRepository = delegate.metadataRepository
  }

  case class Live(ds: DataSource, clock: Clock.Service) extends Service { self =>

    import Queries.ctx
    import ctx._

    implicit val dsEnv = Implicit(Has(ds))

    override def metadataRepository: MetadataRepository = new MetadataRepository(ds, "events") {
      override def delete(metadata: Metadata): Task[Unit] = self.delete(metadata.id).unit
    }

    override def createNoTxn(event: Event): QIO[Event] = {
      Queries.create(event)
    }

    override def get(id: UUID): IO[CatchEmAllException, Event] =
      Queries
        .get(id)
        .implicitly
        .mapToDependencyErrors

    override def delete(id: UUID): IO[CatchEmAllException, Unit] = {
      val query = Queries.delete(id)

      ctx
        .transaction(query)
        .txnRetry()
        .implicitly
        .mapToDependencyErrors
        .unit
    }

    override def stream(
      from: Option[UUID] = None,
      batchSize: Int = DefaultBatchSize,
      durationBetweenPolls: Duration = DefaultDurationBetweenPolls
    ): IO[CatchEmAllException, ZStream[Any, CatchEmAllException, Event]] = {
      val query = for {
        orderedId <- from.map(uuid => ZIO.succeed(Queries.uuidToLong(uuid))).getOrElse(Queries.getLastId.map(_.headOption.getOrElse(0L)))
      } yield {
        zio.stream.Stream
          .unfoldM(orderedId) { (from: Long) =>
            (for {
              batchOpt <- Queries
                .stream(from = from, take = batchSize)
                .implicitly
                .timeout(10.second)
                .mapToDependencyErrors
              batch     = batchOpt.getOrElse(Seq())
              _        <- zio.clock.sleep(durationBetweenPolls).when(batch.isEmpty)
            } yield {
              Option(batch.map(_._1), batch.lastOption.map(_._2).getOrElse(from))
            })
          }
          .flatMap(ZStream.fromIterable(_))
      }.provideLayer(ZEnv.live)

      query.implicitly.mapToDependencyErrors
    }
  }
}
