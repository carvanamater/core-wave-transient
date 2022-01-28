package com.carvana.core.wave

import cats.implicits._
import cats.{Apply, FlatMap}
import diffson.Jsony
import diffson.jsonmergepatch._
import diffson.jsonpatch._
import diffson.jsonpointer._
import zio.json.ast.Json
import zio.json.{JsonCodec, JsonDecoder, JsonEncoder}

import scala.util.Try

package object zdiffson {

  implicit object JsonyZioJson extends Jsony[Json] {

    override def makeObject(fields: Map[String, Json]): Json = {
      Json.Obj(fields.toSeq: _*)
    }

    override def fields(json: Json): Option[Map[String, Json]] = {
      json match {
        case json: Json.Obj => Option(json.fields.toMap)
        case _              => None
      }
    }

    override def makeArray(values: Vector[Json]): Json = {
      Json.Arr(values: _*)
    }

    override def array(json: Json): Option[Vector[Json]] = {
      json match {
        case json: Json.Arr => Option(json.elements.toVector)
        case _              => None
      }
    }

    override def Null: Json = Json.Null

    override def show(t: Json): String = t.toJsonPretty

    override def eqv(x: Json, y: Json): Boolean = x == y
  }

  implicit val pointerJsonCodec: JsonCodec[Pointer] = JsonCodec(
    JsonEncoder.string.contramap((p: Pointer) => p.show),
    JsonDecoder.string.mapOrFail(
      v =>
        Pointer
          .parse[Try](v)
          .toEither
          .fold(
            v => Left(v.getMessage),
            Right(_)
          )
    )
  )

  private def jsonGet[T: JsonDecoder](fields: Map[String, Json], field: String): Either[String, T] = {
    fields.get(field).map { v =>
      implicitly[JsonDecoder[T]].fromJsonAST(v)
    }.getOrElse(Left(s"missing field $field"))
  }

  private def jsonGetOption[T: JsonDecoder](fields: Map[String, Json], field: String): Either[String, Option[T]] = {
    fields.get(field).map { v =>
      implicitly[JsonDecoder[T]].fromJsonAST(v).map(Option(_))
    }.getOrElse(Right(None))
  }

  implicit val operationJsonCodec: JsonCodec[Operation[Json]] = JsonCodec(
    {
      Json.encoder.contramap {
        case Add(path, value) =>
          Json.Obj(
            "op" -> Json.Str("add"),
            "path" -> Json.Str(path.show),
            "value" -> value
          )
        case Remove(path, Some(old)) =>
          Json.Obj("op" -> Json.Str("remove"), "path" -> Json.Str(path.show), "old" -> old)
        case Remove(path, None) =>
          Json.Obj("op" -> Json.Str("remove"), "path" -> Json.Str(path.show))
        case Replace(path, value, Some(old)) =>
          Json.Obj("op" -> Json.Str("replace"), "path" -> Json.Str(path.show), "value" -> value, "old" -> old)
        case Replace(path, value, None) =>
          Json.Obj("op" -> Json.Str("replace"), "path" -> Json.Str(path.show), "value" -> value)
        case Move(from, path) =>
          Json.Obj("op" -> Json.Str("move"), "from" -> Json.Str(from.show), "path" -> Json.Str(path.show))
        case Copy(from, path) =>
          Json.Obj("op" -> Json.Str("copy"), "from" -> Json.Str(from.show), "path" -> Json.Str(path.show))
        case Test(path, value) =>
          Json.Obj("op" -> Json.Str("test"), "path" -> Json.Str(path.show), "value" -> value)
      }
    }, {
      val A = Apply[Either[String, *]]
      val F = FlatMap[Either[String, *]]

      Json.decoder.mapOrFail {
        case Json.Obj(fields) =>
          val fieldsMap = fields.toMap

          fieldsMap
            .get("op")
            .flatMap {
              case Json.Str(v) => Option(v)
              case _ => None
            }
            .map {
              case "add" =>
                A.map2(jsonGet[Pointer](fieldsMap, "path"), jsonGet[Json](fieldsMap, "value"))(Add[Json])
                  .leftMap(_ => "missing 'path' or 'value' field")
              case "remove" =>
                A.map2(jsonGet[Pointer](fieldsMap, "path"), jsonGetOption[Json](fieldsMap, "old"))(Remove[Json])
                  .leftMap(_ => "missing 'path' field")
              case "replace" =>
                A.map3(jsonGet[Pointer](fieldsMap, "path"), jsonGet[Json](fieldsMap, "value"), jsonGetOption[Json](fieldsMap, "old"))(Replace[Json] _)
                  .leftMap(_ => "missing 'path' or 'value' field")
              case "move" =>
                A.map2(jsonGet[Pointer](fieldsMap, "from"), jsonGet[Pointer](fieldsMap, "path"))(Move[Json])
                  .leftMap(_ => "missing 'from' or 'path' field")
              case "copy" =>
                A.map2(jsonGet[Pointer](fieldsMap, "from"), jsonGet[Pointer](fieldsMap, "path"))(Copy[Json])
                  .leftMap(_ => "missing 'from' or 'path' field")
              case "test" =>
                A.map2(jsonGet[Pointer](fieldsMap, "path"), jsonGet[Json](fieldsMap, "value"))(Test[Json])
                  .leftMap(_ => "missing 'path' or 'value' field")
              case other =>
                Left(s"unknown JSON patch operation $other")
            }
            .getOrElse(Left("invalid JSON patch operation"))
        case _ => Left("invalid JSON patch operation")
      }
    }
  )

  implicit val jsonPatchCodec: JsonCodec[JsonPatch[Json]] =
    JsonCodec(
      implicitly[JsonEncoder[List[Operation[Json]]]].contramap(patch => patch.ops),
      implicitly[JsonDecoder[List[Operation[Json]]]].map(ops => JsonPatch(ops)),
    )

  implicit val jsonMergePatchCodec: JsonCodec[JsonMergePatch[Json]] =
    JsonCodec(
      Json.encoder.contramap(patch => patch.toJson),
      Json.decoder.map {
        case json: Json.Obj => JsonMergePatch.Object(json.fields.toMap)
        case json => JsonMergePatch.Value(json)
      }
    )
}
