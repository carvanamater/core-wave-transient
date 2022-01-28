package com.carvana.core.wave.webhook

import com.typesafe.config.ConfigFactory
import io.jsonwebtoken.SignatureAlgorithm
import io.jsonwebtoken.security.Keys
import zio.{Has, ZIO, ZLayer}
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.config.ConfigDescriptor
import zio.config.typesafe.TypesafeConfig
import zio.config.toKebabCase

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.security.spec.{PKCS8EncodedKeySpec, X509EncodedKeySpec}
import java.security.{KeyFactory, KeyPair}
import java.util.Base64

sealed trait SecurityUtil

object SecurityUtil {

  case class KeyPairConfig(algorithm: String, privateKeyFile: String, publicKeyFile: String)
  case class KeyPairsConfig(keyPairs: Map[String, KeyPairConfig] = Map())

  case class KeyPairs(keyPairs: Map[String, KeyPair] = Map())

  implicit val configDescriptor: ConfigDescriptor[KeyPairsConfig] = {
    descriptor[KeyPairsConfig].mapKey(toKebabCase)
  }

  val liveKeyPairs: ZLayer[Any, Nothing, Has[KeyPairs]] = {
    TypesafeConfig
      .fromTypesafeConfig(ConfigFactory.load().getConfig("com.carvana.core.security"), configDescriptor)
      .orDie >>> (for {
      config <- ZIO.service[KeyPairsConfig]
    } yield {
      KeyPairs(config.keyPairs.map { case (k, v) => (k, loadKeyPair(v.algorithm, v.privateKeyFile, v.publicKeyFile)) })
    }).toLayer
  }

  def liveKeyPair(name: String): ZLayer[Any, NoSuchElementException, Has[KeyPair]] =
    liveKeyPairs >>> (for {
      keyPairs <- ZIO.service[KeyPairs]
    keyPair <- ZIO.fromOption(keyPairs.keyPairs.get(name)).orElseFail(new NoSuchElementException(s"missing key with name $name"))
    } yield keyPair).toLayer

  // load a keypair generated using the createKeyPairFiles method
  def loadKeyPair(algorithm: String, privateKeyPath: String, publicKeyPath: String): KeyPair = {

    val privateContents = new String(Files.readAllBytes(new File(privateKeyPath).toPath), StandardCharsets.UTF_8)
    val publicContents = new String(Files.readAllBytes(new File(publicKeyPath).toPath), StandardCharsets.UTF_8)

    val keyFactory = KeyFactory.getInstance(algorithm)

    val privateKeySpec = new PKCS8EncodedKeySpec(Base64.getDecoder.decode(privateContents))
    val publicKeySpec = new X509EncodedKeySpec(Base64.getDecoder.decode(publicContents))

    val privateKey = keyFactory.generatePrivate(privateKeySpec)
    val publicKey = keyFactory.generatePublic(publicKeySpec)

    new KeyPair(publicKey, privateKey)
  }

  // for generating keypairs, see: https://github.com/jwtk/jjwt#asymmetric-keys
  // this method will generate a key pair and write the private/public key to their respective paths
  // these keys can then be loaded using the loadKeyPair method
  def createKeyPairFiles(signatureAlgorithm: SignatureAlgorithm, privateKeyPath: String, publicKeyPath: String): Unit = {
    val keyPair = Keys.keyPairFor(signatureAlgorithm)

    Files.write(new File(privateKeyPath).toPath, Base64.getEncoder.encode(keyPair.getPrivate.getEncoded))
    Files.write(new File(publicKeyPath).toPath, Base64.getEncoder.encode(keyPair.getPublic.getEncoded))
  }
}
