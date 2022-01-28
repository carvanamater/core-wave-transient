package com.carvana.core.wave.rest

import com.carvana.core.wave.jresource
import io.getquill.MappedEncoding
import sttp.tapir.{Schema, Validator}
import zio.config.{ConfigDescriptor, toKebabCase}
import zio.json.{JsonCodec, JsonDecoder, JsonEncoder}

trait ModelEnumValue {
  def name: String
}

trait ModelEnumCompanion[T <: ModelEnumValue] {
  def ALL_VALUES: Set[T]
  val LOOKUP: Map[String, T] = ALL_VALUES.map(x => (x.name, x)).toMap

  def forName(name: String): T = LOOKUP.get(name).orElse(forUnknown(name)).getOrElse(throw new Exception(s"Bad enum value ($name)"))

  def forUnknown(name: String): Option[T] = None

  implicit val configDescriptor: ConfigDescriptor[T] =
    ConfigDescriptor.string.transform[T](
      forName,
      _.name
    ).mapKey(toKebabCase)

  implicit val jsonDecoder: JsonDecoder[T] = JsonDecoder.string.mapOrFail { x =>
    LOOKUP.get(x).orElse(forUnknown(x)).toRight(s"Bad enumeration value ($x)")
  }
  implicit val jsonEncoder: JsonEncoder[T] = JsonEncoder.string.contramap(_.name)
  implicit val jsonCodec: JsonCodec[T] = JsonCodec.apply(jsonEncoder, jsonDecoder)
  implicit val schema: jresource.Schema[T] = jresource.Schema.Str().enumeration(ALL_VALUES.map(_.name).toSeq: _*).as[T](forName, _.name)

  implicit val tapirSchema: Schema[T] = Schema.string.validate(
    Validator.enumeration(ALL_VALUES.toList, x => Some(x.name))
  )

  implicit val quillEncoder: MappedEncoding[T, String] = MappedEncoding(_.name)
  implicit val quillDecoder: MappedEncoding[String, T] = MappedEncoding(forName)

  implicit val quillSetEncoder: MappedEncoding[Set[T], Seq[String]] = MappedEncoding(_.toSeq.map(_.name))
  implicit val quillSetDecoder: MappedEncoding[Seq[String], Set[T]] = MappedEncoding(_.map(forName).toSet)
}
