package com.carvana.core.wave.model

import com.carvana.core.common.{CompactUUID, ResourceName}
import com.carvana.core.validation.{FieldError, NameValidators}
import com.carvana.core.wave.jresource.Schema
import com.carvana.core.wave.rest.ResourceNameUtil
import io.getquill.MappedEncoding
import zio.json.{JsonCodec, JsonDecoder, JsonEncoder}
import zio.prelude.Validation

import java.util.UUID

trait SimpleResourceId[R] {
  def uuid: UUID

  def prefix: String

  override def toString: String = s"$prefix${CompactUUID(uuid)}"
}

trait SimpleResourceIdCompanion[R <: SimpleResourceId[R]] extends SimplePrefix[R] {

  type IdType = R

  def apply(uuid: UUID): R

  def validate(name: String): Validation[FieldError, R] = {
    NameValidators.validateSimple("name", prefix, name).map(s => apply(s.uuid))
  }

  implicit val quillEncoder: MappedEncoding[R, UUID] = MappedEncoding(_.uuid)

  implicit val quillDecoder: MappedEncoding[UUID, R] = MappedEncoding(apply)

  implicit val jsonCodec: JsonCodec[R] =
    JsonCodec(
      implicitly[JsonEncoder[String]].contramap(v => ResourceName.Simple(prefix, CompactUUID(v.uuid)).toString),
      implicitly[JsonDecoder[String]].mapOrFail(
        v =>
          ResourceNameUtil.parseSimpleName(v).flatMap { name =>
            if (name.prefix != prefix) Left(s"invalid prefix ${name.prefix}") else Right(apply(name.uuid))
          }
      )
    )

  implicit val jschema: Schema[IdType] = Schema.Str().pattern(s"$prefix[0-9a-f]{32}").as[IdType](
    v => ResourceNameUtil.parseSimpleName(v).flatMap { name =>
      if (name.prefix != prefix) Left(s"invalid prefix ${name.prefix}") else Right(apply(name.uuid))
    }.toOption.get,
    v => ResourceName.Simple(prefix, CompactUUID(v.uuid)).toString
  )
}
