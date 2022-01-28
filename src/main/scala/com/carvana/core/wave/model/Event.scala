package com.carvana.core.wave.model

import com.carvana.core.wave.jresource.{DeriveSchema, Schema}
import com.carvana.core.wave.webhook.{Transform, TransformChain}
import zio.{URIO, ZIO}
import zio.clock.Clock
import zio.json.{DeriveJsonCodec, JsonCodec, JsonEncoder}
import Schema.jsonValue
import zio.json.ast.Json
import zio.json._

import java.util.UUID

object Event extends ResourceCompanion[Event] {

  def create[R : JsonEncoder](kind: String, transformChain: TransformChain[Transform.Post], after: R): Event =
    Event(
      operation = Operation.Post,
      kind = kind,
      original = Option(transformChain.original.request),
      transforms = optionalSeq(transformChain.links),
      before = None,
      after = Option(after.toJsonAST.toOption.get)
    )

  def put[R : JsonEncoder](kind: String, transformChain: TransformChain[Transform.Put], before: R, after: R): Event =
    Event(
      operation = Operation.Put,
      kind = kind,
      original = Option(transformChain.original.request),
      transforms = optionalSeq(transformChain.links),
      before = Option(before.toJsonAST.toOption.get),
      after = Option(after.toJsonAST.toOption.get)
    )

  def patch[R : JsonEncoder](kind: String, transformChain: TransformChain[Transform.Patch], before: R, after: R): Event =
    Event(
      operation = Operation.Patch,
      kind = kind,
      original = Option(transformChain.original.request),
      transforms = optionalSeq(transformChain.links),
      before = Option(before.toJsonAST.toOption.get),
      after = Option(after.toJsonAST.toOption.get)
    )

  def delete[R : JsonEncoder](kind: String, before: R): Event =
    Event(
      operation = Operation.Delete,
      kind = kind,
      before = Option(before.toJsonAST.toOption.get)
    )

  def optionalSeq[T](seq: Seq[T]): Option[Seq[T]] = if (seq.isEmpty) None else Option(seq)

  implicit object Id extends SimpleResourceIdCompanion[Id] {
    override def prefix: String = "TSE"
  }

  implicit override lazy val simplePrefix: SimplePrefix[Event] = SimplePrefix.generic(Id.prefix)

  case class Id(override val uuid: UUID) extends SimpleResourceId[Id] {
    override def prefix: String = Id.prefix
  }

  implicit val j: JsonCodec[Json] = JsonCodec(Json.encoder, Json.decoder)
  implicit override lazy val jsonCodec: JsonCodec[Event] = DeriveJsonCodec.gen[Event]
  override implicit lazy val jschema: Schema[Event] = DeriveSchema.gen[Event]

  override def replace(a: Event, b: Event): URIO[Clock, Event] =
    ZIO.succeed {
      a.copy(
        metadata = Metadata.replace(a.metadata, b.metadata),
        operation = b.operation,
        kind = b.kind,
        before = b.before,
        after = b.after
      )
    }

  override def initialize(a: Event): URIO[Clock, Event] = ZIO.succeed(a)

  override def updateMetadata(a: Event)(f: Metadata => Metadata): Event = a.copy(metadata = f(a.metadata))
}

case class Event(
  override val metadata: Metadata = Metadata(),
  operation: Operation = Operation.Unknown,
  kind: String = "",
  original: Option[Json] = None,
  transforms: Option[Seq[TransformChain.Link]] = None,
  before: Option[Json] = None,
  after: Option[Json] = None
) extends Resource
