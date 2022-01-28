package com.carvana.core.wave.rest.controller

import com.carvana.core.wave.webhook.RequestSigner
import zio.json.{DeriveJsonCodec, JsonCodec}
import zio.macros.accessible
import zio.{Has, UIO, ZIO, ZLayer}

import java.nio.charset.StandardCharsets
import java.util.Base64

@accessible[SecurityController.Service]
object SecurityController {

  object PublicKey {
    implicit val jsonCodec: JsonCodec[PublicKey] = DeriveJsonCodec.gen[PublicKey]
  }

  case class PublicKey(key: String)

  val live: ZLayer[Has[RequestSigner.Service], Nothing, Has[Service]] = {
    (for {
      requestSigner <- ZIO.service[RequestSigner.Service]
    } yield Live(requestSigner)).toLayer
  }

  trait Service {
    def getPublicKey: UIO[PublicKey]
  }

  case class Live(requestSigner: RequestSigner.Service) extends Service {

    override def getPublicKey: UIO[PublicKey] = {
      requestSigner.publicKey.map { key =>
        PublicKey(key = new String(Base64.getEncoder.encode(key.getEncoded), StandardCharsets.UTF_8))
      }
    }
  }
}
