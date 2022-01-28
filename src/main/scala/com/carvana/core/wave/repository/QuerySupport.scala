package com.carvana.core.wave.repository

import com.carvana.core.NotFoundException
import com.carvana.core.common.{CompactUUID, ResourceName}
import com.carvana.core.db.quill.{DataException, DefaultQuillContext}
import com.carvana.core.wave.model.{Condition, Conditions, Target}
import com.carvana.core.wave.repository.QuerySupport.Postgres
import io.getquill.{MappedEncoding, NamingStrategy, PostgresZioJdbcContext, SnakeCase}
import zio.json._
import zio.json.ast.Json
import zio.{IO, ZIO}

import java.sql.{SQLException, Types}
import java.util.UUID

object QuerySupport {

  class Postgres[N <: NamingStrategy](naming: N) extends DefaultQuillContext[N](naming) with CustomEncoding

  val DEFAULT_CONTEXT: Postgres[SnakeCase.type] = new Postgres[SnakeCase.type](SnakeCase)
}

case class RawJson(json: Json)

trait CustomEncoding {
  this: PostgresZioJdbcContext[_] =>

  implicit val dbConditionCodec: JsonCodec[Condition] = DeriveJsonCodec.gen[Condition]

  implicit val rawJsonEncoder: Encoder[RawJson] =
    encoder(java.sql.Types.OTHER, (index, value, row) =>
      row.setObject(index, value.json.toJson, java.sql.Types.OTHER))

  implicit val rawJsonDecoder: Decoder[RawJson] =
    decoder(row => index => RawJson(Json.decoder.decodeJson(row.getObject(index).toString).toOption.get))

  implicit val mapStringEncoder: MappedEncoding[Map[String, String], RawJson] = MappedEncoding(map => RawJson(implicitly[JsonEncoder[Map[String, String]]].toJsonAST(map).toOption.get))

  implicit val mapStringDecoder: MappedEncoding[RawJson, Map[String, String]] = MappedEncoding(
    json => implicitly[JsonDecoder[Map[String, String]]].fromJsonAST(json.json).toOption.get
  )

  implicit val conditionListEncoder: MappedEncoding[Conditions, RawJson] = MappedEncoding(list => RawJson(implicitly[JsonEncoder[List[Condition]]].toJsonAST(list).toOption.get))

  implicit val conditionListDecoder: MappedEncoding[RawJson, Conditions] = MappedEncoding(
    json => implicitly[JsonDecoder[List[Condition]]].fromJsonAST(json.json).toOption.get
  )

}

class NotFoundSQLException(message: String = "", cause: Throwable = None.orNull) extends SQLException(message, cause)

trait QuerySupport {
  val ctx: QuerySupport.Postgres[SnakeCase.type] = QuerySupport.DEFAULT_CONTEXT

  def singleRow[A](list: Seq[A])(msg: => String): IO[DataException.SingleRowException, A] = {
    list.headOption.map(ZIO.succeed(_)).getOrElse(ZIO.fail(DataException.SingleRowException(msg)))
  }

  def optionalRow[A](list: Seq[A]): Option[A] = {
    list.headOption
  }
}

trait EventQuerySupport extends QuerySupport {
  def uuidToLong(uuid: UUID): Long = uuid.getMostSignificantBits
  def longToUuid(long: Long): UUID = new UUID(long, 0L)

  override val ctx: QuerySupport.Postgres[SnakeCase.type] = new Postgres[SnakeCase.type ](SnakeCase) {
    override implicit val uuidEncoder: Encoder[UUID] = encoder(Types.BIGINT, row => (idx, uuid) => row.setLong(idx, uuidToLong(uuid)))
    override implicit val uuidDecoder: Decoder[UUID] = decoder(row => idx => longToUuid(row.getLong(idx)))

  }

}
