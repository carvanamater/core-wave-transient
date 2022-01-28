package com.carvana.core.wave.model

import com.carvana.core.common.{CompactUUID, ResourceName}
import com.carvana.core.validation.Patterns.SIMPLE_PATTERN
import com.carvana.core.wave.jresource._
import com.carvana.core.wave.rest.ResourceNameUtil
import io.getquill.Embedded
import zio.json.{DeriveJsonCodec, JsonCodec, JsonDecoder, JsonEncoder, jsonField}

import java.time.Instant
import java.util.UUID

object Metadata {

  implicit def jsonCodec[R: SimplePrefix]: JsonCodec[Metadata] = {
    implicit val uuidCodec: JsonCodec[UUID] =
      JsonCodec(
        JsonEncoder.string.contramap(v => ResourceName.simpleFromCompact(implicitly[SimplePrefix[R]].prefix, CompactUUID(v)).toString),
        JsonDecoder.string.mapOrFail(uuid => ResourceNameUtil.parseSimpleName(uuid).map(_.uuid))
      )

    DeriveJsonCodec.gen[Metadata]
  }

  implicit def jschema[R: SimplePrefix]: Schema[Metadata] = {
    implicit val uuid: Schema[UUID] = Schema.Str().pattern(SIMPLE_PATTERN.toString()).as[UUID](
      str => ResourceNameUtil.parseSimpleName(str).map(_.uuid).toOption.get,
      v => ResourceName.simpleFromCompact(implicitly[SimplePrefix[R]].prefix, CompactUUID(v)).toString
    )
    DeriveSchema.gen[Metadata]
  }

  def replace(a: Metadata, b: Metadata): Metadata = {
    a.copy(
      labels = b.labels,
      annotations = b.annotations,
      expireTime = b.expireTime
    )
  }
}

case class Metadata(
  @jsonField("name")
  id: UUID = Common.UUID_ZERO,

  @jsMinLength(1)
  @jsMaxLength(32)
  @jsMaxItems(16)
  labels: Map[String, String] = Map(),

  @jsMinLength(1)
  @jsMaxLength(1024 * 4)
  @jsMaxItems(32)
  annotations: Map[String, String] = Map(),

  @jsFormat(Format.Email)
  owner: String = "",

  @jsFormat(Format.Email)
  createUser: String = "",
  createTime: Instant = Instant.MIN,

  @jsFormat(Format.Email)
  updateUser: String = "",
  updateTime: Instant = Instant.MIN,
  expireTime: Option[Instant] = None,
) extends Embedded
