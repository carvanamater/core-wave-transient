package com.carvana.core.wave.zdiffson

import diffson.jsonpatch.Operation
import sttp.tapir.Schema
import zio.json.{JsonCodec, JsonDecoder, JsonEncoder}
import zio.json.ast.Json

import scala.util.Try

sealed trait Patch {
  def apply(json: Json): Try[Json]

  def toJson: Json
}

object Patch {

  case class InvalidPatchException(msg: String = "", cause: Throwable = None.orNull) extends Exception(msg, cause)

  def unsafeFromJson(json: Json): Patch = json match {
    case Json.Obj(fields) =>
      MergePatch(diffson.jsonmergepatch.JsonMergePatch.Object(fields.toMap))
    case json: Json.Arr   =>
      implicitly[JsonDecoder[List[Operation[Json]]]]
        .fromJsonAST(json)
        .map { operations =>
          JsonPatch(diffson.jsonpatch.JsonPatch(operations))
        }
        .fold(
          msg => Left(InvalidPatchException(msg)),
          Right(_)
        )
        .toTry
        .get
    case _                => throw InvalidPatchException("must be merge patch or json patch content")
  }

  case class JsonPatch(patch: diffson.jsonpatch.JsonPatch[Json]) extends Patch {

    override def apply(json: Json): Try[Json] = patch[Try](json)

    override def toJson: Json = patch.ops.toJsonAST.toOption.get
  }

  case class MergePatch(patch: diffson.jsonmergepatch.JsonMergePatch.Object[Json]) extends Patch {

    override def apply(json: Json): Try[Json] = patch[Try](json)

    override def toJson: Json = patch.toJson
  }

  implicit val jsonEncoder: JsonEncoder[Patch] = Json.encoder.contramap[Patch](_.toJson)

  implicit val jsonDecoder: JsonDecoder[Patch] = Json.decoder.mapOrFail {
    case Json.Obj(fields) =>
      Right(MergePatch(diffson.jsonmergepatch.JsonMergePatch.Object(fields.toMap)))
    case json: Json.Arr   =>
      implicitly[JsonDecoder[List[Operation[Json]]]].fromJsonAST(json).map { operations =>
        JsonPatch(diffson.jsonpatch.JsonPatch(operations))
      }
    case _                => Left("invalid patch or JSON merge")
  }

  implicit val jsonCodec: JsonCodec[Patch] =
    JsonCodec[Patch](
      jsonEncoder,
      jsonDecoder
    )

  implicit val schema: Schema[Patch] = sttp.tapir.json.zio.schemaForZioJsonValue.as[Patch]

}
