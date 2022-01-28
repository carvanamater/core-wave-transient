package com.carvana.core.wave.jresource

import sttp.tapir
import sttp.tapir.Schema.SName
import sttp.tapir.SchemaType.SProduct
import sttp.tapir.{FieldName, SchemaType, Validator}
import zio.Chunk
import zio.json.JsonDecoder.UnsafeJson
import zio.json.ast.Json
import zio.json.internal.{Lexer, RetractReader}
import zio.json.{JsonCodec, JsonDecoder, JsonEncoder, JsonError}
import sttp.tapir.json.zio.{schemaForZioJsonObject, schemaForZioJsonValue}

import java.time.Instant
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

trait Format {
  def name: String

  def validate(value: String): Either[String, String]
}

object Format {

  case object DateTime extends Format {
    val PATTERN: Regex = """(?i)^\d{4}(-\d\d(-\d\d(T\d\d:\d\d(:\d\d)?(\.\d+)?(([+-]\d\d:\d\d)|Z)?)?)?)?$""".r

    override def name: String = "date-time"

    override def validate(value: String): Either[String, String] = {
      if (PATTERN.matches(value)) Right(value) else Left("must be valid ISO8601 date-time")
    }
  }

  case object Email extends Format {
    val PATTERN: Regex = """(?i)@""".r.unanchored

    override def name: String = "email"

    override def validate(value: String): Either[String, String] = {
      if (PATTERN.matches(value)) Right(value) else Left("must be valid email")
    }
  }

  case object Uri extends Format {
    override def name: String = "uri"

    override def validate(value: String): Either[String, String] = sttp.model.Uri.parse(value).map(_.toString)
  }
}

sealed trait Schema[A] {
  def annotate(annotation: SchemaAnnotation): Schema[A]

  def jsonCodec: JsonCodec[Json] =
    JsonCodec(
      jsonEncoder,
      jsonDecoder
    )

  def jsonEncoder: JsonEncoder[Json] = Json.encoder.contramap(v => jsonDecoder.fromJsonAST(v).toOption.get)

  def jsonDecoder: JsonDecoder[Json]

  def isOptional: Boolean

  def nonOptional: Schema[A]

  def description: Option[String]

  def description(description: String): Schema[A]

  def default: Option[Json]

  def default(default: Json): Schema[A]

  def as[B](
    encode: A => B,
    decode: B => A
  ): Schema.As[A, B] =
    Schema.As(
      base = this,
      encode = encode,
      decode = decode
    )

  def tapirSchema: tapir.Schema[A]

}

object Schema {

  implicit val string: Schema.Str   = Schema.Str()
  implicit val instant: Schema[Instant] = Schema.Str().format(Format.DateTime).as(
    Instant.parse,
    _.toString
  )

  implicit val int: Schema[Int]       = Schema.Num(kind = Num.Int).as[Int](
    _.intValue,
    BigDecimal(_)
  )
  implicit val long: Schema[Long]     = Schema.Num(kind = Num.Long).as[Long](
    _.longValue,
    BigDecimal(_)
  )
  implicit val float: Schema[Float]   = Schema.Num(kind = Num.Float).as[Float](
    _.floatValue,
    BigDecimal(_)
  )
  implicit val double: Schema[Double] = Schema.Num(kind = Num.Double).as[Double](
    _.doubleValue,
    BigDecimal(_)
  )

  implicit val jsonValue: Schema.AnyVal = Schema.AnyVal()

  implicit val jsonObj: Schema.AnyObj = Schema.AnyObj()

  implicit def option[A](implicit schema: Schema[A]): Schema[Option[A]] = schema.as[Option[A]](
    Option(_),
    _.get
  )

  implicit def iterable[A, T[X] <: Iterable[X]](implicit schema: Schema[A], codec: JsonCodec[T[A]]): Schema.Arr[A, T] =
    Arr[A, T](schema = schema)

  implicit def iterableMap[A, T[String, X] <: Iterable[(String, X)]](implicit schema: Schema[A], codec: JsonCodec[T[String, A]]): Schema.RMap[A, T] =
    RMap[A, T](schema = schema)

  case class As[A, B](
    base: Schema[A],
    encode: A => B,
    decode: B => A
  ) extends Schema[B] {

    override def annotate(annotation: SchemaAnnotation): Schema[B] =
      copy(base = base.annotate(annotation))

    override def jsonDecoder: JsonDecoder[Json] = base.jsonDecoder

    override def isOptional: Boolean = base.isOptional

    override def nonOptional: Schema[B] =
      copy(base = base.nonOptional)

    override def description: Option[String] = base.description

    override def description(description: String): Schema[B] =
      copy(base = base.description(description))

    override def default: Option[Json] = base.default

    override def default(default: Json): Schema[B] =
      copy(base = base.default(default))

    override def tapirSchema: tapir.Schema[B] =
      base.tapirSchema.map[B](v => Option(encode(v)))(decode)
  }

  object Num {
    sealed trait Kind

    case object Int        extends Kind
    case object Long       extends Kind
    case object Float      extends Kind
    case object Double     extends Kind
    case object BigDecimal extends Kind
  }

  case class Num(
    kind: Num.Kind,
    override val isOptional: Boolean = true,
    override val description: Option[String] = None,
    override val default: Option[Json] = None
  ) extends Schema[BigDecimal] {

    override lazy val jsonDecoder: JsonDecoder[Json] =
      Json.Num.decoder.mapOrFail(v => validate(v.value).map(Json.Num(_)))

    override def description(description: String): Num =
      copy(description = Option(description))

    override def default(default: Json): Num =
      copy(default = Option(default))

    override def annotate(annotation: SchemaAnnotation): Num =
      // TODO
      this

    def validate(value: BigDecimal): Either[String, BigDecimal] =
      // TODO
      Right(value)

    override def nonOptional: Num = copy(isOptional = false)

    override def tapirSchema: tapir.Schema[BigDecimal] = tapir.Schema.schemaForBigDecimal
  }

  case class Str(
    override val isOptional: Boolean = true,
    override val description: Option[String] = None,
    override val default: Option[Json] = None,
    enumeration: Option[List[String]] = None,
    minLength: Option[Int] = None,
    maxLength: Option[Int] = None,
    pattern: Option[String] = None,
    format: Option[Format] = None
  ) extends Schema[String] {

    lazy val patternR: Option[Regex] = pattern.map(_.r)

    override lazy val jsonDecoder: JsonDecoder[Json] = Json.Str.decoder.mapOrFail(v => validate(v.value).map(Json.Str(_)))

    override def nonOptional: Str = copy(isOptional = false)

    override def description(description: String): Str =
      copy(description = Option(description))

    override def default(default: Json): Str =
      copy(default = Option(default))

    override def annotate(annotation: SchemaAnnotation): Str = annotation match {
      case ann: jsEnumeration => enumeration(ann.values: _*)
      case ann: jsMinLength   => minLength(ann.min)
      case ann: jsMaxLength   => maxLength(ann.max)
      case ann: jsPattern     => pattern(ann.pattern)
      case ann: jsFormat      => format(ann.format)
      case _                  => this
    }

    def enumeration(values: String*): Str = copy(enumeration = Option(values.toList.distinct))

    def minLength(v: Int): Str = copy(minLength = Option(v))

    def maxLength(v: Int): Str = copy(maxLength = Option(v))

    def pattern(v: String): Str = copy(pattern = Option(v))

    def format(v: Format): Str = copy(format = Option(v))

    def validate(v: String): Either[String, String] = {
      for {
        v2 <- validateMinLength(v)
        v3 <- validateMinLength(v2)
        v4 <- validateMinLength(v3)
        v5 <- validateMinLength(v4)
        v6 <- validateEnumeration(v5)
        v7 <- validateFormat(v6)
      } yield v7
    }

    def validateMinLength(v: String): Either[String, String] = {
      minLength.map(m => if (m < v.length) Right(v) else Left(s"minimum length of $m")).getOrElse(Right(v))
    }

    def validateMaxLength(v: String): Either[String, String] = {
      minLength.map(m => if (m < v.length) Right(v) else Left(s"minimum length of $m")).getOrElse(Right(v))
    }

    def validatePattern(v: String): Either[String, String] = {
      patternR.map(r => if (r.matches(v)) Right(v) else Left(s"match $r")).getOrElse(Right(v))
    }

    def validateFormat(v: String): Either[String, String] = {
      format.map(f => f.validate(v)).getOrElse(Right(v))
    }

    def validateEnumeration(v: String): Either[String, String] = {
      enumeration.map(e => if (e.contains(v)) Right(v) else Left(s"must be in (${enumeration.mkString(",")})")).getOrElse(Right(v))
    }

    override def tapirSchema: tapir.Schema[String] = {
      var s = tapir.Schema.string[String].copy(isOptional = isOptional)
      s = enumeration.fold(s)(v => s.validate(Validator.enumeration(v)))
      s = minLength.fold(s)(v => s.validate(Validator.minLength(v)))
      s = maxLength.fold(s)(v => s.validate(Validator.maxLength(v)))
      s = pattern.fold(s)(v => s.validate(Validator.pattern(v)))
      s = format.fold(s)(v => s.format(v.name))
      s
    }
  }

  case class Arr[A, T[X] <: Iterable[X]](
    schema: Schema[A],
    override val isOptional: Boolean = true,
    override val description: Option[String] = None,
    override val default: Option[Json] = None,
    minItems: Option[Int] = None,
    maxItems: Option[Int] = None
  ) extends Schema[T[A]] {

    override def description(description: String): Arr[A, T] =
      copy(description = Option(description))

    override def default(default: Json): Arr[A, T] =
      copy(default = Option(default))

    override def nonOptional: Arr[A, T] = copy(isOptional = false)

    def minItems(v: Int): Arr[A, T] = copy(minItems = Option(v))

    def maxItems(v: Int): Arr[A, T] = copy(maxItems = Option(v))

    override lazy val jsonDecoder: JsonDecoder[Json] =
      new JsonDecoder[Json] {
        lazy val itemDecoder: JsonDecoder[Json] = schema.jsonDecoder

        override def unsafeDecode(trace: List[JsonError], in: RetractReader): Json = {
          var chunk = Chunk[Json]()

          Lexer.char(trace, in, '[')
          var i: Int = 0
          if (Lexer.firstArrayElement(in)) while ({
            {
              if (maxItems.exists(_ < i))
                throw UnsafeJson(JsonError.Message(s"maximum of ${maxItems.get} items") :: trace)

              val trace_ = JsonError.ArrayAccess(i) :: trace
              chunk = chunk :+ itemDecoder.unsafeDecode(trace_, in)
              i += 1
            };
            Lexer.nextArrayElement(trace, in)
          }) ()

          if (minItems.exists(_ > i))
            throw UnsafeJson(JsonError.Message(s"minimum of ${minItems.get} items") :: trace)
          Json.Arr(chunk)
        }

        override def fromJsonAST(json: Json): Either[String, Json] = json match {
          case arr @ Json.Arr(els) =>
            if (minItems.exists(_ > els.length)) {
              Left(s"minimum of ${minItems.get} items")
            } else if (maxItems.exists(_ < els.length)) {
              Left(s"maximum of ${maxItems.get} items")
            } else {
              var i = 0
              els
                .foldLeft[Either[String, Unit]](Right(())) { (acc, el) =>
                  val r = acc
                    .flatMap(_ => itemDecoder.fromJsonAST(el))
                    .fold(
                      err => Left(s"[$i]($err)"),
                      _ => Right(())
                    )
                  i += 1
                  r
                }
                .map(_ => arr)
            }
          case _                   => Left("not an array")
        }
      }

    override def annotate(annotation: SchemaAnnotation): Arr[A, T] = annotation match {
      case ann: jsMinItems => minItems(ann.min)
      case ann: jsMaxItems => maxItems(ann.max)
      case _ => copy(schema = schema.annotate(annotation))
    }

    override def tapirSchema: tapir.Schema[T[A]] = {
      var s = tapir.Schema.schemaForIterable[A, T](schema.tapirSchema).copy(isOptional = isOptional)
      s = minItems.fold(s)(v => s.validate(Validator.minSize(v)))
      s = maxItems.fold(s)(v => s.validate(Validator.maxSize(v)))
      s
    }
  }

  case class RMap[A, T[String, X] <: Iterable[(String, X)]](
    schema: Schema[A],
    override val isOptional: Boolean = true,
    override val description: Option[String] = None,
    override val default: Option[Json] = None,
    minItems: Option[Int] = None,
    maxItems: Option[Int] = None
  ) extends Schema[T[String, A]] {

    override def description(description: String): RMap[A, T] =
      copy(description = Option(description))

    override def default(default: Json): RMap[A, T] =
      copy(default = Option(default))

    override def nonOptional: RMap[A, T] = copy(isOptional = false)

    def minItems(v: Int): RMap[A, T] = copy(minItems = Option(v))

    def maxItems(v: Int): RMap[A, T] = copy(maxItems = Option(v))

    override lazy val jsonDecoder: JsonDecoder[Json] =
      new JsonDecoder[Json] {
        lazy val itemDecoder: JsonDecoder[Json] = schema.jsonDecoder

        override def unsafeDecode(trace: List[JsonError], in: RetractReader): Json = {
          var chunk = Chunk[(String, Json)]()

          Lexer.char(trace, in, '{')
          var i = 0
          if (Lexer.firstField(trace, in)) {
            while ({
              {
                val field  = Lexer.string(trace, in).toString

                if (maxItems.exists(_ < i))
                  throw UnsafeJson(JsonError.Message(s"maximum of ${maxItems.get} items") :: trace)

                val trace_ = JsonError.ObjectAccess(field) :: trace
                Lexer.char(trace_, in, ':')
                val value  = itemDecoder.unsafeDecode(trace_, in)
                chunk = chunk :+ (field -> value)
                i += 1
              }; Lexer.nextField(trace, in)
            }) ()
          }

          if (minItems.exists(_ > i))
            throw UnsafeJson(JsonError.Message(s"minimum of ${minItems.get} items") :: trace)

          Json.Obj(chunk)
        }

        override def fromJsonAST(json: Json): Either[String, Json] = json match {
          case obj @ Json.Obj(fields) =>
            if (minItems.exists(_ < fields.length)) {
              Left(s"(minimum of ${minItems.get} items)")
            } else if (maxItems.exists(_ < fields.length)) {
              Left(s"(maximum of ${maxItems.get} items)")
            } else {
              fields
                .foldLeft[Either[String, Unit]](Right(())) { case (acc, (k, v)) =>
                  acc
                    .flatMap(_ => itemDecoder.fromJsonAST(v))
                    .fold(
                      err => Left(s"[$k]($err)"),
                      _ => Right(())
                    )
                }
                .map(_ => obj)
            }
          case _                      =>
            Left("Not an object")
        }
      }

    override def annotate(annotation: SchemaAnnotation): RMap[A, T] = annotation match {
      case ann: jsMinItems => minItems(ann.min)
      case ann: jsMaxItems => maxItems(ann.max)
      case _ => copy(schema = schema.annotate(annotation))
    }

    override def tapirSchema: tapir.Schema[T[String, A]] = {
      var s = tapir.Schema(
        schemaType = SchemaType.SOpenProduct[T[String, A], A](schema.tapirSchema)(t => t.toMap)
      )
      s = minItems.fold(s)(v => s.validate(Validator.minSize(v)))
      s = maxItems.fold(s)(v => s.validate(Validator.maxSize(v)))
      s
    }
  }

  object Product {

    trait Field[A] {
      type FieldType

      def schema: Schema[FieldType]

      def get: A => Option[FieldType]
    }

    object Field {

      def apply[A, F](_schema: Schema[F], _get: A => Option[F]): Field[A] =
        new Field[A] {
          type FieldType = F

          override def schema: Schema[F] = _schema

          override def get: A => Option[F] = _get
        }
    }
  }

  case class Product[A](
    name: Option[String] = None,
    override val isOptional: Boolean = true,
    override val description: Option[String] = None,
    override val default: Option[Json] = None,
    maxExtra: Option[Int] = Option(32),
    fields: immutable.ListMap[String, Product.Field[A]] = immutable.ListMap[String, Product.Field[A]]()
  ) extends Schema[A] { self =>

    override def description(description: String): Product[A] =
      copy(description = Option(description))

    override def default(default: Json): Product[A] =
      copy(default = Option(default))

    def field[F](name: String, field: Product.Field[A]): Product[A] =
      copy(fields = fields + (name -> field))

    override def nonOptional: Product[A]                            = copy(isOptional = false)

    override lazy val jsonDecoder: JsonDecoder[Json] =
      new JsonDecoder[Json] {

        def unsafeDecode(trace: List[JsonError], in: RetractReader): Json = {
          var extra: Int                   = 0
          var chunk: Chunk[(String, Json)] = Chunk()

          Lexer.char(trace, in, '{')
          if (Lexer.firstField(trace, in))
            while ({
              {
                val fieldName = Lexer.string(trace, in).toString
                val trace_    = JsonError.ObjectAccess(fieldName) :: trace
                Lexer.char(trace_, in, ':')

                fields.get(fieldName) match {
                  case Some(field) =>
                    chunk = chunk :+ (fieldName, field.schema.jsonDecoder.unsafeDecode(trace_, in))
                  case None        =>
                    extra += 1
                    if (maxExtra.exists(_ < extra))
                      throw UnsafeJson(JsonError.Message(s"maximum of ${maxExtra.get} extra fields") :: trace_)

                    chunk = chunk :+ (fieldName, Json.decoder.unsafeDecode(trace_, in))
                }
              }; Lexer.nextField(trace, in)
            }) ()

          Json.Obj(chunk)
        }

        final override def fromJsonAST(json: Json): Either[String, Json] =
          json match {
            case Json.Obj(fields) =>
              var chunk      = Chunk[(String, Json)]()
              val failures   = new mutable.LinkedHashSet[String]
              var extra: Int = 0

              for ((key, value) <- fields) {
                self.fields.get(key) match {
                  case Some(field) =>
                    field.schema.jsonDecoder.fromJsonAST(value) match {
                      case Left(err)    => failures += s".$key($err)"
                      case Right(value) => chunk = chunk :+ (key -> value)
                    }
                  case None        =>
                    extra += 1
                    if (maxExtra.exists(_ < extra)) {
                      failures += s".$key(maximum of ${maxExtra.get} extra fields)"
                    } else {
                      chunk = chunk :+ (key -> value)
                    }
                }
              }

              if (failures.nonEmpty)
                Left(failures.mkString(", "))
              else
                Right(Json.Obj(chunk))
            case _                => Left("Not an object")
          }
      }

    override def annotate(annotation: SchemaAnnotation): Product[A] = this

    override def tapirSchema: tapir.Schema[A] = {
      val s = SProduct(
        fields.toList.map { case (name, field) =>
          SchemaType.SProductField[A, field.FieldType](FieldName(name), field.schema.tapirSchema, field.get)
        }
      )

      tapir.Schema[A](
        schemaType = s,
        name = name.map(name => SName(name)),
        isOptional = isOptional,
        description = description
      )
    }
  }

  case class AnyVal(
    override val isOptional: Boolean = true,
    override val description: Option[String] = None,
    override val default: Option[Json] = None
  ) extends Schema[Json] {

    override def annotate(annotation: SchemaAnnotation): AnyVal = this

    override lazy val jsonDecoder: JsonDecoder[Json] = Json.decoder

    override def nonOptional: Schema[Json] =
      copy(isOptional = false)

    override def description(description: String): AnyVal =
      copy(description = Option(description))

    override def default(default: Json): AnyVal =
      copy(default = Option(default))

    override def tapirSchema: tapir.Schema[Json] =
      schemaForZioJsonValue
  }

  case class AnyObj(
    override val isOptional: Boolean = true,
    override val description: Option[String] = None,
    override val default: Option[Json] = None
  ) extends Schema[Json.Obj] {

    override def annotate(annotation: SchemaAnnotation): AnyObj = this

    override lazy val jsonDecoder: JsonDecoder[Json] = Json.decoder

    override def nonOptional: AnyObj =
      copy(isOptional = false)

    override def description(description: String): AnyObj =
      copy(description = Option(description))

    override def default(default: Json): AnyObj =
      copy(default = Option(default))

    override def tapirSchema: tapir.Schema[Json.Obj] = schemaForZioJsonObject
  }
}
