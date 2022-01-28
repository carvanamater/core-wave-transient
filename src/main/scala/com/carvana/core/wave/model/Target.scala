package com.carvana.core.wave.model

import com.carvana.core.wave.jresource.Schema
import com.carvana.core.wave.model.Target.Part.Wildcard
import io.getquill.{Embedded, MappedEncoding}
import zio.json.{JsonCodec, JsonDecoder, JsonEncoder}
import zio.config.{ConfigDescriptor, toKebabCase}

import scala.language.implicitConversions
import scala.util.Try

object Target {

  sealed trait Part

  object Part {

    implicit def apply(value: String): Part = value match {
      case "*" => Wildcard
      case _   => Name(value)
    }

    case class Name(value: String) extends Part {
      override def toString: String = value
    }

    case object Wildcard extends Part {
      override def toString: String = "*"
    }
  }

  def parse(value: String): Try[Target] = Try(Target(value.split('/').toList.reverse.map(Part.apply)))

  def parseUnsafe(value: String): Target = parse(value).get

  val Root: Target = Target()

  implicit val jsonCodec: JsonCodec[Target] = JsonCodec(
    JsonEncoder.string.contramap(_.toString),
    JsonDecoder.string.mapOrFail(s => parse(s).toEither.fold(v => Left(v.getMessage), Right(_)))
  )
  implicit val jschema: Schema[Target] = Schema.Str().as[Target](
    s => parse(s).toEither.fold(v => Left(v.getMessage), Right(_)).toOption.get,
    _.toString
  )

  implicit val quillEncoder: MappedEncoding[Target, String] = MappedEncoding(_.toString)
  implicit val quillDecoder: MappedEncoding[String, Target] = MappedEncoding(Target.parseUnsafe)

  implicit val configDescriptor: ConfigDescriptor[Target] =
    ConfigDescriptor.string
      .transform[Target](
        parseUnsafe,
        _.toString
      )
      .mapKey(toKebabCase)
}

case class Target(target: List[Target.Part] = List()) extends Embedded {
  def /(part: Target.Part): Target = copy(target = part :: target)

  def parent: Target = Target(target.tail)

  def all: List[Target] = {
    if (target.nonEmpty) {
      listWithWildcard
    } else {
      List()
    }
  }

  def listWithWildcard: List[Target] = {
    target.lastOption match {
      case Some(Target.Part.Wildcard) =>
        this :: parent.listWithWildcard
      case None =>
        List(Target(List(Target.Part.Wildcard)))
      case _ =>
        val p = parent
        this :: (p / Wildcard) :: p.listWithWildcard
    }
  }

  override def toString: String = target.reverse.mkString("/")
}
