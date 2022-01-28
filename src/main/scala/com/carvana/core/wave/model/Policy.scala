package com.carvana.core.wave.model

import com.carvana.core.wave.jresource.{DeriveSchema, Schema}
import zio.{URIO, ZIO}
import zio.clock.Clock
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.util.UUID

object Policy extends ResourceCompanion[Policy] {

  val Grant: Operation = Operation.Custom("grant")

  implicit object Id extends SimpleResourceIdCompanion[Id] {
    override def prefix: String = "PO"
  }

  case class Id(override val uuid: UUID) extends SimpleResourceId[Id] {
    override def prefix: String = Id.prefix
  }

  implicit override lazy val jsonCodec: JsonCodec[Policy] = DeriveJsonCodec.gen[Policy]
  implicit override lazy val simplePrefix: SimplePrefix[Policy] = SimplePrefix.generic(Id.prefix)
  override implicit val jschema: Schema[Policy] = DeriveSchema.gen[Policy]

  object ListRequest {
    val DefaultSize: Int = 10

    object Filters {

      implicit val jsonCodec: JsonCodec[Filters] = DeriveJsonCodec.gen[Filters]
    }

    case class Filters(
      owner: Option[String] = None,
      operation: Option[Operation] = None,
      target: Option[Target] = None,
      subject: Option[String] = None,
      targets: Option[List[Target]] = None
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

  case class ListResponse(policies: Seq[Policy] = Seq(), nextPageToken: Option[String] = None)

  override def replace(a: Policy, b: Policy): URIO[Clock, Policy] = ZIO.succeed {
    a.copy(
      metadata = Metadata.replace(a.metadata, b.metadata),
      operations = b.operations,
      target = b.target,
      subject = b.subject
    )
  }

  override def initialize(a: Policy): URIO[Clock, Policy] = ZIO.succeed(a)

  override def updateMetadata(a: Policy)(f: Metadata => Metadata): Policy = a.copy(metadata = f(a.metadata))
}

case class Policy(
  override val metadata: Metadata = Metadata(),
  operations: Set[Operation] = Set(),
  target: Target = Target(),
  subject: String = ""
) extends Resource
