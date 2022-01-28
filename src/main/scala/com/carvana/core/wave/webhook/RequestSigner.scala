package com.carvana.core.wave.webhook

import zio.{Has, IO, UIO, ZIO, ZLayer}
import zio.macros.accessible

import java.nio.charset.StandardCharsets
import java.security.{GeneralSecurityException, KeyPair, PrivateKey, PublicKey, Signature}
import java.util.Base64

sealed trait RequestSigner

@accessible[RequestSigner.Service]
object RequestSigner {

  val Base64Encoder: Base64.Encoder = Base64.getEncoder

  val live: ZLayer[Has[KeyPair], Nothing, Has[Service]] = {
    (for {
      keyPair <- ZIO.service[KeyPair]
    } yield Live(keyPair)).toLayer
  }

  trait Service {
    def signRequest(request: String): IO[GeneralSecurityException, String]

    def publicKey: UIO[PublicKey]
  }

  case class Live(keyPair: KeyPair) extends Service {

    override def signRequest(request: String): IO[GeneralSecurityException, String] =
      ZIO.effect {
        val signer = Signature.getInstance("SHA256withRSA")
        signer.initSign(keyPair.getPrivate)
        signer.update(request.getBytes(StandardCharsets.UTF_8))

        new String(Base64Encoder.encode(signer.sign()), StandardCharsets.UTF_8)
      }.refineToOrDie[GeneralSecurityException]

    override def publicKey: UIO[PublicKey] = ZIO.succeed(keyPair.getPublic)
  }

}
