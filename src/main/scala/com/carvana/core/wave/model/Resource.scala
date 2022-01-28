package com.carvana.core.wave.model

import com.carvana.core.wave.jresource.Schema
import zio.URIO
import zio.clock.Clock
import zio.json.JsonCodec

trait Resource {
  def metadata: Metadata
}

trait ResourceCompanion[R <: Resource] {
  implicit val jsonCodec: JsonCodec[R]

  implicit val jschema: Schema[R]

  implicit val resourceCompanion: ResourceCompanion[R] = this

  implicit def simplePrefix: SimplePrefix[R]

  implicit def metadataJsonCodec: JsonCodec[Metadata] = Metadata.jsonCodec(simplePrefix)
  implicit def metadataSchema: Schema[Metadata] = Metadata.jschema

  def replace(a: R, b: R): URIO[Clock, R]

  def initialize(a: R): URIO[Clock, R]

  def updateMetadata(a: R)(f: Metadata => Metadata): R

  def name(a: R): String = simplePrefix.name(a.metadata.id)
}
