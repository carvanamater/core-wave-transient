package com.carvana.core.wave.model

import com.carvana.core.wave.jresource.{DeriveSchema, Schema}
import io.getquill.MappedEncoding
import zio.{URIO, ZIO}
import zio.clock.Clock
import zio.json.{DeriveJsonCodec, JsonCodec, JsonDecoder, JsonEncoder}
import zio.json.ast.Json

import java.util.UUID

object EventStream extends ResourceCompanion[EventStream] {

  implicit object Id extends SimpleResourceIdCompanion[Id] {
    override def prefix: String = "ES"
  }

  implicit override lazy val simplePrefix: SimplePrefix[EventStream] = SimplePrefix.generic(Id.prefix)

  case class Id(override val uuid: UUID) extends SimpleResourceId[Id] {
    override def prefix: String = Id.prefix
  }

  object Offset {
    val Zero: Offset = Offset(new UUID(0L, 0L))

    implicit val jsonCodec: JsonCodec[Offset] =
      JsonCodec(
        JsonEncoder.uuid.contramap(_.streamOffset),
        JsonDecoder.uuid.map(Offset(_))
      )

    implicit val jschema: Schema[Offset] = Schema.Str().as[Offset](
      v => Offset(UUID.fromString(v)),
      _.streamOffset.toString
    )

    implicit val quillEncoder: MappedEncoding[Offset, Long] =
      MappedEncoding(_.streamOffset.getMostSignificantBits)

    implicit val quillDecoder: MappedEncoding[Long, Offset] =
      MappedEncoding(v => Offset(new UUID(v, 0L)))
  }

  case class Offset(streamOffset: UUID)

  object ListRequest {
    val DefaultSize: Int = 10

    object Filters {

      implicit val jsonCodec: JsonCodec[Filters] = DeriveJsonCodec.gen[Filters]
    }

    case class Filters(
      owner: Option[String] = None,
      name: Option[String] = None
    )
  }

  case class ListRequest(
    size: Int = ListRequest.DefaultSize,
    filters: ListRequest.Filters = ListRequest.Filters(),
    pageToken: Option[String] = None
  )

  object ListResponse {

    implicit val jsonCodec: JsonCodec[ListResponse] = DeriveJsonCodec.gen[ListResponse]

    implicit val jschema: Schema[ListResponse] = DeriveSchema.gen[ListResponse]
  }

  case class ListResponse(eventStreams: Seq[EventStream] = Seq(), nextPageToken: Option[String] = None)

  implicit val j: JsonCodec[Json]                              = JsonCodec(Json.encoder, Json.decoder)
  implicit override lazy val jsonCodec: JsonCodec[EventStream] = DeriveJsonCodec.gen[EventStream]
  implicit override lazy val jschema: Schema[EventStream]      = DeriveSchema.gen[EventStream]

  override def replace(a: EventStream, b: EventStream): URIO[Clock, EventStream] =
    ZIO.succeed {
      a.copy(
        metadata = Metadata.replace(a.metadata, b.metadata),
        streamOffset = b.streamOffset
      )
    }

  override def initialize(a: EventStream): URIO[Clock, EventStream] = ZIO.succeed(a)

  override def updateMetadata(a: EventStream)(f: Metadata => Metadata): EventStream = a.copy(metadata = f(a.metadata))
}

case class EventStream(
  override val metadata: Metadata = Metadata(),
  name: String = "",
  streamOffset: EventStream.Offset = EventStream.Offset.Zero
) extends Resource
