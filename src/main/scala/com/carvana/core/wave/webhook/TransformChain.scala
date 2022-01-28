package com.carvana.core.wave.webhook

import com.carvana.core.wave.jresource.{DeriveSchema, Format, Schema}
import com.carvana.core.wave.zdiffson.Patch
import io.getquill.MappedEncoding
import sttp.model.Uri
import zio.json._

import scala.language.implicitConversions
import scala.util.Try

object TransformChain {
  object Link {
    implicit val uriCodec: JsonCodec[Uri] = {
      JsonCodec(
        JsonCodec.string.contramap(_.toString),
        JsonCodec.string.mapOrFail(Uri.parse)
      )
    }
    implicit val uriJschema: Schema[Uri] = Schema.Str().format(Format.Uri).as[Uri](Uri.unsafeParse, _.toString)
    implicit val patchSchema: Schema[Patch] = Schema.AnyVal().as[Patch](Patch.unsafeFromJson, _.toJson)
    implicit val jsonCodec: JsonCodec[Link] = DeriveJsonCodec.gen[Link]
    implicit val jschema: Schema[Link] = DeriveSchema.gen[Link]
    implicit val quillEncoder: MappedEncoding[Link, String] = MappedEncoding(_.toJson)
    implicit val quillDecoder: MappedEncoding[String, Link] = MappedEncoding(json => jsonCodec.decodeJson(json).toOption.get)
  }

  case class Link(uri: Uri, patch: Patch)

  implicit def apply[T <: Transform : TransformCompanion](original: T): TransformChain[T] = TransformChain(original = original, current = original)
}

case class TransformChain[T <: Transform : TransformCompanion](original: T, current: T, links: Seq[TransformChain.Link] = Seq()) {
  def tryWithLink(uri: Uri, patch: Patch): Try[TransformChain[T]] = tryWithLink(TransformChain.Link(uri, patch))

  def tryWithLink(link: TransformChain.Link): Try[TransformChain[T]] = {
    implicitly[TransformCompanion[T]].tryMapRequest(current)(json => link.patch(json)).map { next =>
      copy(current = next, links = links :+ link)
    }
  }
}
