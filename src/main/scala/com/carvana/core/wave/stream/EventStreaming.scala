package com.carvana.core.wave.stream

import com.carvana.core.CatchEmAllException
import com.carvana.core.wave.auth.Authentication
import com.carvana.core.wave.model.{Event, EventStream, Metadata}
import com.carvana.core.wave.repository.{EventRepository, EventStreamRepository}
import com.carvana.core.wave.rest.ResourceUtil
import zio.clock.Clock
import zio.macros.accessible
import zio.stream.{ZStream, ZTransducer}
import zio.{Has, IO, ZIO, ZLayer}

@accessible[EventStreaming.Service]
object EventStreaming {

  type Reqs = Has[EventStreamRepository.Service] with Has[EventRepository.Service] with Has[Authentication.Service] with Clock

  trait OffsetBatch {
    def eventStream: EventStream

    def offset: EventStream.Offset

    def commit: IO[CatchEmAllException, Unit]

    def ++(event: Event): DefaultOffsetBatch

    def ++(offset: EventStream.Offset): DefaultOffsetBatch
  }

  case class DefaultOffsetBatch(
    override val eventStream: EventStream,
    override val offset: EventStream.Offset,
    eventStreamRepository: EventStreamRepository.Service
  ) extends OffsetBatch {

    override def commit: IO[CatchEmAllException, Unit] =
      eventStreamRepository.commit(eventStream.metadata.id, offset).unless(offset == EventStream.Offset.Zero)

    override def ++(event: Event): DefaultOffsetBatch =
      this.++(EventStream.Offset(event.metadata.id))

    override def ++(offset: EventStream.Offset): DefaultOffsetBatch =
      if (offset.streamOffset.compareTo(this.offset.streamOffset) < 0) {
        this
      } else {
        copy(offset = EventStream.Offset(offset.streamOffset))
      }
  }

  def eventOffsetBatches(offsetBatch: OffsetBatch): ZTransducer[Any, Nothing, Event, OffsetBatch] =
    ZTransducer.foldLeft[Event, OffsetBatch](offsetBatch)(_ ++ _)

  val live: ZLayer[Reqs, Nothing, Has[Service]] =
    (for {
      eventStreamRepository <- ZIO.service[EventStreamRepository.Service]
      eventRepository       <- ZIO.service[EventRepository.Service]
      authentication        <- ZIO.service[Authentication.Service]
      clock                 <- ZIO.service[Clock.Service]
    } yield Live(
      eventStreamRepository = eventStreamRepository,
      eventRepository = eventRepository,
      authentication = authentication,
      clock = clock
    )).toLayer

  trait Service {
    def stream(name: String): IO[CatchEmAllException, (OffsetBatch, ZStream[Any, CatchEmAllException, Event])]
  }

  case class Live(
    eventStreamRepository: EventStreamRepository.Service,
    eventRepository: EventRepository.Service,
    authentication: Authentication.Service,
    clock: Clock.Service
  ) extends Service {

    private val provide = Has(authentication) ++ Has(clock)

    override def stream(name: String): IO[CatchEmAllException, (OffsetBatch, ZStream[Any, CatchEmAllException, Event])] =
      for {
        existingO   <- eventStreamRepository
          .list(
            EventStream.ListRequest(
              size = 1,
              filters = EventStream.ListRequest.Filters(name = Option(name))
            )
          )
          .map(_.eventStreams.headOption)
        eventStream <- existingO.map(ZIO.succeed(_)).getOrElse {
          val eventStream = EventStream(
            metadata = Metadata(),
            name = name
          )

          (for {
            eventStream2 <- EventStream.initialize(eventStream)
            eventStream3 <- ResourceUtil.createAuthenticated(eventStream2, owner = Option("system@nosidelines.io"))
            eventStream4 <- eventStreamRepository.create(eventStream3)
          } yield eventStream4).provide(provide)
        }
        stream      <- eventRepository
          .stream(from = Option(eventStream.streamOffset.streamOffset))
        offsetBatch  = DefaultOffsetBatch(eventStream = eventStream, offset = eventStream.streamOffset, eventStreamRepository = eventStreamRepository)
      } yield (offsetBatch, stream)
  }
}
