package com.carvana.core.wave.webhook

import zio.json.ast.Json
import zio.json.{jsonDiscriminator, jsonHint, DeriveJsonCodec, JsonCodec}

import scala.util.Try

@jsonDiscriminator("operation")
sealed trait Transform {
  def kind: String

  def operation: String
}

sealed trait TransformCompanion[T <: Transform] {
  def withValue(obj: T)(value: Option[Json]): T

  def tryMapRequest(obj: T)(f: Json => Try[Json]): Try[T]
}

object Transform {

  implicit object Post extends TransformCompanion[Post] {
    override def withValue(obj: Post)(value: Option[Json]): Post = value.fold(obj)(v => obj.copy(request = v))

    override def tryMapRequest(obj: Post)(f: Json => Try[Json]): Try[Post] =
      f(obj.request).map(r => obj.copy(request = r))
  }

  @jsonHint("post")
  case class Post(override val kind: String, request: Json) extends Transform {
    override def operation: String = "post"
  }

  implicit object Put extends TransformCompanion[Put] {

    override def withValue(obj: Put)(value: Option[Json]): Put =
      value.fold(obj)(v => obj.copy(request = v))

    override def tryMapRequest(obj: Put)(f: Json => Try[Json]): Try[Put] =
      f(obj.request).map(r => obj.copy(request = r))
  }

  @jsonHint("put")
  case class Put(override val kind: String, id: String, request: Json) extends Transform {
    override def operation: String = "put"
  }

  implicit object Patch extends TransformCompanion[Patch] {

    override def withValue(obj: Patch)(value: Option[Json]): Patch =
      value.fold(obj)(v => obj.copy(request = v))

    override def tryMapRequest(obj: Patch)(f: Json => Try[Json]): Try[Patch] =
      f(obj.request).map(r => obj.copy(request = r))
  }

  @jsonHint("patch")
  case class Patch(override val kind: String, id: String, request: Json) extends Transform {
    override def operation: String = "patch"
  }

  implicit object Delete extends TransformCompanion[Delete] {

    override def withValue(obj: Delete)(value: Option[Json]): Delete =
      value.fold(obj)(v => obj.copy(request = v))

    override def tryMapRequest(obj: Delete)(f: Json => Try[Json]): Try[Delete] =
      f(obj.request).map(r => obj.copy(request = r))
  }

  @jsonHint("delete")
  case class Delete(override val kind: String, id: String, request: Json) extends Transform {
    override def operation: String = "delete"
  }

  def post(kind: String, request: Json): Post =
    Transform.Post(kind = kind, request = request)

  def put(kind: String, id: String, request: Json): Put =
    Transform.Put(kind = kind, id = id, request = request)

  def patch(kind: String, id: String, request: Json): Patch =
    Transform.Patch(kind = kind, id = id, request = request)

  def delete(kind: String, id: String, request: Json): Delete =
    Transform.Delete(kind = kind, id = id, request = request)

  implicit val jCodec: JsonCodec[Json] = JsonCodec(Json.encoder, Json.decoder)
  implicit val jsonCodec: JsonCodec[Transform] = DeriveJsonCodec.gen[Transform]
}
