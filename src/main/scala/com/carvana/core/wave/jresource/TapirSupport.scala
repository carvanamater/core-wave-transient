package com.carvana.core.wave.jresource

import sttp.tapir
import sttp.tapir.{anyFromUtf8StringBody, EndpointIO}
import sttp.tapir.json.zio.zioCodec
import zio.json.JsonCodec

trait TapirSupport {

  def jsonResourceBody[R](implicit schema: Schema[R]): EndpointIO.Body[String, JsonResource[R]] = {

    implicit val jsonCodec: JsonCodec[JsonResource[R]]      = JsonCodec(
      schema.jsonEncoder.contramap(_.json),
      schema.jsonDecoder.mapOrFail(json => JsonResource.make[R](json))
    )
    implicit val tapirSchema: tapir.Schema[JsonResource[R]] = schema.tapirSchema.as[JsonResource[R]]

    anyFromUtf8StringBody(zioCodec)
  }

  def jsonResourceBodyUnsafe[R](implicit schema: Schema[R]): EndpointIO.Body[String, JsonResource[R]] = {
    implicit val jsonCodec: JsonCodec[JsonResource[R]]      = JsonCodec(
      schema.jsonEncoder.contramap(_.json),
      schema.jsonDecoder.map(json => JsonResource.makeUnsafe[R](json))
    )
    implicit val tapirSchema: tapir.Schema[JsonResource[R]] = schema.tapirSchema.as[JsonResource[R]]

    anyFromUtf8StringBody(zioCodec)
  }
}

object TapirSupport extends TapirSupport
