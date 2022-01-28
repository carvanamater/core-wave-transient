package com.carvana.core.wave.model

import com.carvana.core.wave.model.Paging.Token.TokenError
import zio.{IO, ZIO}
import zio.json.ast.Json
import zio.json._

import java.nio.charset.StandardCharsets
import java.util.Base64

object Paging {

  private val Base64Encoder: Base64.Encoder = Base64.getEncoder
  private val Base64Decoder: Base64.Decoder = Base64.getDecoder

  object Token {
    sealed abstract class TokenError(message: String = "", cause: Throwable = None.orNull) extends Exception(message, cause)

    object TokenError {
      case class ParsingFailed(message: String = "", cause: Throwable = None.orNull) extends TokenError(message, cause)
      case class InvalidPageToken(message: String = "", cause: Throwable = None.orNull) extends TokenError(message, cause)
    }

    implicit val jCodec: JsonCodec[Json] = JsonCodec(Json.encoder, Json.decoder)
    implicit val jsonCodec: JsonCodec[Token] = DeriveJsonCodec.gen[Token]

    def fromToken(token: String): IO[TokenError.ParsingFailed, Token] = {
      ZIO
        .fromEither(new String(Base64Decoder.decode(token.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8).fromJson[Token])
        .mapError(msg => TokenError.ParsingFailed(msg))
    }

  }

  case class Token(name: String, filters: Json) {

    def toToken: String = {
      val str = this.toJson
      new String(Base64Encoder.encode(str.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8)
    }

    def validateToken[F : JsonCodec](filters: F): IO[TokenError.InvalidPageToken, Unit] = {
      ZIO.unless(filters.toJsonAST.contains(this.filters)) {
        ZIO.fail(TokenError.InvalidPageToken("list filters must match across requests"))
      }
    }
  }
}
