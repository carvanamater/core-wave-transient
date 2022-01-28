package com.carvana.core.wave.rest.controller

import com.carvana.core._
import com.carvana.core.wave.auth.Authentication
import com.carvana.core.wave.jresource.{JsonResource, Schema}
import com.carvana.core.wave.model.{Resource, ResourceCompanion}
import com.carvana.core.wave.zdiffson._
import diffson.jsonpointer.Pointer
import zio.json.ast.Json
import zio.json.{JsonCodec, JsonDecoder, JsonEncoder}
import zio.{Has, IO, ZIO}

import scala.util.Try

case class InvalidResourceException(message: String = "", cause: Throwable = None.orNull) extends BadClientRequestException(message, cause)

trait Controller {

  val OwnerPointer: Pointer = Pointer.Root / "metadata" / "owner"

  def decode[R: JsonDecoder](json: Json): IO[CatchEmAllException, R] = {
    ZIO
    .fromEither(implicitly[JsonDecoder[R]].fromJsonAST(json))
      .mapError(msg => new InvalidResourceException(s"invalid resource $msg") with SafeException)
  }

  def encode[R: JsonEncoder](resource: R): IO[CatchEmAllException, Json] = {
    ZIO
      .fromEither(implicitly[JsonEncoder[R]].toJsonAST(resource))
      .mapError(msg => new InvalidResourceException(s"invalid resource $msg") with SafeException)
  }

  def getOwnerForRequest[R <: Resource](request: JsonResource[R]): ZIO[Has[Authentication.Service], CatchEmAllException, String] = {
    OwnerPointer
      .evaluate[Try, Json](request.json)
      .toOption
      .flatMap {
        case Json.Str(value) => Option(value)
        case _               => None
      }
      .map(ZIO.succeed(_))
      .getOrElse(Authentication.subject)
  }

  implicit class ZioCatchEmAllOps[R, A](zio: ZIO[R, CatchEmAllException, A]) {

    def wrapNotFound: ZIO[R, CatchEmAllException, A] = zio.mapError {
      case err: NotFoundException => new NotFoundRequestException("resource does not exist", err) with SafeException
      case err                    => err
    }
  }
}
