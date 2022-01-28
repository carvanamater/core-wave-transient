package com.carvana.core.wave.repository

import com.carvana.core.CatchEmAllException
import com.carvana.core.common.{CompactUUID, ResourceName}
import com.carvana.core.validation.{CommonValidators, NameValidators}
import com.carvana.core.wave.model.{EventStream, Paging}
import com.carvana.core.wave.repository.Repository.FromPageToken
import io.getquill.Ord
import zio.{Has, IO, ZIO, ZLayer}
import zio.json._
import zio.macros.accessible
import java.io.Closeable
import java.util.UUID

import io.getquill.context.qzio.ImplicitSyntax.{Implicit, ImplicitSyntaxOps}
import javax.sql.DataSource

@accessible[EventStreamRepository.Service]
object EventStreamRepository {

  object Queries extends QuerySupport {
    import ctx._

    lazy val eventStreams = quote(querySchema[EventStream]("event_streams"))

    def getQ(id: UUID) = quote(eventStreams.filter(_.metadata.id == lift(id)))

    def get(id: UUID) = ctx.run(getQ(id)).flatMap(Queries.singleRow(_)(s"could not find event stream with id $id"))

    def getFromQ(id: UUID) = quote(eventStreams.filter(_.metadata.id == lift(id)).map(v => (v.metadata.createTime, v.metadata.id)))

    def getFrom(id: UUID) = ctx.run(getFromQ(id)).map(_.headOption)

    def createQ(eventStream: EventStream) = {
      quote(eventStreams.insert(lift(eventStream)).returningGenerated(o => (o.metadata.id, o.metadata.createTime, o.metadata.updateTime)))
    }

    def create(eventStream: EventStream) =
      ctx.run(createQ(eventStream)).map { case (id, createTime, updateTime) =>
        eventStream.copy(
          metadata = eventStream.metadata.copy(id = id, createTime = createTime, updateTime = updateTime)
        )
      }

    def commitQ(id: UUID, offset: EventStream.Offset) =
      quote(
        eventStreams
          .filter(_.metadata.id == lift(id))
          .update(_.streamOffset -> lift(offset))
      )

    def commit(id: UUID, offset: EventStream.Offset) =
      ctx.run(commitQ(id, offset))

    def listQ(list: EventStream.ListRequest, from: FromPageToken) = {
      eventStreams.dynamic
        .filterOpt(list.filters.owner)((a, u) => a.metadata.owner == u)
        .filterOpt(list.filters.name)((a, u) => quote(a.name == u))
        .filterIf(from.filter)(a => {
          quote(a.metadata.createTime < lift(from.fromCreateTime)) ||
            (quote(a.metadata.createTime == lift(from.fromCreateTime)) && infix"${a.metadata.id} < ${lift(from.fromId)}".as[Boolean])
        })
        .take(list.size)
        .sortBy(a => (a.metadata.createTime, a.metadata.id))(Ord(Ord.desc, Ord.desc))
    }

    def list(list: EventStream.ListRequest, from: FromPageToken) = ctx.run(listQ(list, from))
  }

  val live: ZLayer[Has[DataSource with Closeable], Nothing, Has[Service]] =
    (for {
      ds <- ZIO.service[DataSource with Closeable]
    } yield Live(ds)).toLayer

  trait Service {
    def get(id: UUID): IO[CatchEmAllException, EventStream]
    def create(eventStream: EventStream): IO[CatchEmAllException, EventStream]
    def commit(id: UUID, offset: EventStream.Offset): IO[CatchEmAllException, Unit]
    def list(list: EventStream.ListRequest): IO[CatchEmAllException, EventStream.ListResponse]
  }

  case class Live(ds: DataSource) extends Service { self =>

    import Queries.ctx
    import ctx._

    implicit val env = Implicit(Has(ds))

    override def get(id: UUID): IO[CatchEmAllException, EventStream] = {
      val query = Queries.get(id)

      query.implicitly.mapToDependencyErrors
    }

    override def create(eventStream: EventStream): IO[CatchEmAllException, EventStream] = {
      val query = Queries.create(eventStream)

      ctx
        .transaction(query)
        .txnRetry()
        .implicitly
        .mapToDependencyErrors
    }

    override def commit(id: UUID, offset: EventStream.Offset): IO[CatchEmAllException, Unit] = {
      val query = Queries.commit(id, offset)

      ctx
        .transaction(query)
        .txnRetry()
        .implicitly
        .mapToDependencyErrors
        .unit
    }

    override def list(list: EventStream.ListRequest): IO[CatchEmAllException, EventStream.ListResponse] = {
      val idZ = list.pageToken.map { t =>
        (for {
          token <- Paging.Token.fromToken(t)
          _     <- token.validateToken(list.filters)
          name  <- CommonValidators
            .validateSafe(NameValidators.validateSimple("name", EventStream.Id.prefix, token.name))("invalid name")
        } yield Option(name.uuid)).mapError(err => Repository.InvalidPageToken(err.getMessage, err))
      }.getOrElse(ZIO.none)

      for {
        id           <- idZ
        query         = for {
          from     <- id.map(i => Queries.getFrom(i)).getOrElse(ZIO.none)
          accounts <- Queries.list(list, FromPageToken(from))
        } yield accounts
        eventStreams <- query.implicitly.mapToDependencyErrors
      } yield {
        EventStream.ListResponse(
          eventStreams = eventStreams,
          nextPageToken = eventStreams.lastOption.map(a =>
            Paging
              .Token(
                name = ResourceName.Simple(EventStream.Id.prefix, CompactUUID(a.metadata.id)).toString,
                filters = list.filters.toJsonAST.toOption.get
              )
              .toToken
          )
        )
      }
    }
  }
}
