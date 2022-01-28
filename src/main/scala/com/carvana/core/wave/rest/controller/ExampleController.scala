package com.carvana.core.wave.rest.controller

import com.carvana.core.{CatchEmAllException, ForbiddenRequestException, SafeException}
import com.carvana.core.wave.zdiffson._
import diffson.jsonpatch.JsonPatch
import diffson.jsonpatch
import diffson.jsonpointer.Pointer
import zio.json.ast.Json
import zio.macros.accessible
import zio.{Has, IO, ZIO, ZLayer}

import scala.util.Try

@accessible[ExampleController.Service]
object ExampleController {

  val LabelsPointer: Pointer = Pointer.Root / "request" / "metadata" / "labels"

  val live: ZLayer[Any, Nothing, Has[Service]] =
    ZIO.succeed(Live()).toLayer

  trait Service {
    def forbid: IO[CatchEmAllException, Unit]
    def addLabel(request: Json, kind: Option[String]): IO[CatchEmAllException, Patch]
  }

  case class Live() extends Service {

    override def forbid: IO[CatchEmAllException, Unit] =
      ZIO.fail(new ForbiddenRequestException(s"example of forbidding with webhook") with SafeException)

    override def addLabel(request: Json, kind: Option[String]): IO[CatchEmAllException, Patch] = ZIO.succeed {
      kind.getOrElse("patch") match {
        case "patch" =>
          if (LabelsPointer.evaluate[Try, Json](request).toOption.exists(_.isInstanceOf[Json.Obj])) {
            Patch.JsonPatch(
              JsonPatch[Json](
                jsonpatch.Add[Json](Pointer.Root / "metadata" / "labels" / "example-webhook-label", Json.Str("json-patch"))
              )
            )
          } else {
            Patch.JsonPatch(
              JsonPatch[Json](
                jsonpatch.Add[Json](Pointer.Root / "metadata" / "labels", Json.Obj("example-webhook-label" -> Json.Str("json-patch")))
              )
            )
          }
        case "merge" | _ =>
          Patch.MergePatch(diffson.jsonmergepatch.JsonMergePatch.Object[Json](Map(
            "metadata" -> Json.Obj(
              "labels" -> Json.Obj("example-webhook-label" -> Json.Str("json-merge"))
            )
          )))
      }
    }
  }
}
