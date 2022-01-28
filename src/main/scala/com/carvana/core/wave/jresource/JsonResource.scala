package com.carvana.core.wave.jresource

import zio.json.JsonCodec
import zio.json.ast.Json

object JsonResource {
  def make[A: Schema](value: Json): Either[String, JsonResource[A]] =
    apply(value).validate

  def makeResource[A: Schema](value: A)
                             (implicit codec: JsonCodec[A]): Either[String, JsonResource[A]] =
    implicitly[JsonCodec[A]].encoder.toJsonAST(value)
      .flatMap(json => makeUnsafe(json).validate)

  def makeUnsafe[A: Schema](value: Json): JsonResource[A] =
    apply(value)

  def makeUnsafeResource[A: Schema](value: A)
                                   (implicit codec: JsonCodec[A]): JsonResource[A] =
    makeUnsafe(implicitly[JsonCodec[A]].encoder.toJsonAST(value).toOption.get)
}

case class JsonResource[A] private(json: Json)(implicit schema: Schema[A]) {
  def update(f: Json => Either[String, Json]): Either[String, JsonResource[A]] =
    updateUnsafe(f).flatMap(_.validate)

  def updateUnsafe(f: Json => Either[String, Json]): Either[String, JsonResource[A]] =
    f(json).map(v => copy(json = v))

  def updateJsonUnsafe(json: Json): JsonResource[A] =
    copy(json = json)

  def decode(implicit codec: JsonCodec[A]): Either[String, A] =
    validateJson.flatMap(codec.decoder.fromJsonAST)

  lazy val validateJson: Either[String, Json] =
    schema.jsonDecoder.fromJsonAST(json)

  def validate: Either[String, JsonResource[A]] =
    validateJson.map(v => copy(json = v))
}

