package com.carvana.core.wave.auth

import com.carvana.core.wave.auth.SystemUsers.SystemUser
import com.carvana.core.wave.model.Policy
import zio.json.{DeriveJsonCodec, JsonCodec}
import zio.{Has, ZIO, ZLayer}
import zio.json._

import scala.io.Source

object SystemUsers {

  object SystemUser {
    implicit val jsonCodec: JsonCodec[SystemUser] = DeriveJsonCodec.gen[SystemUser]
  }

  case class SystemUser(
    username: String,
    password: String,
    policies: Set[Policy]
  )

  val live: ZLayer[Any, Throwable, Has[SystemUsers]] = {
    ZIO.effect {
      val json = Source.fromResource("wave/config/system-users.json", getClass.getClassLoader).getLines().mkString("\n")
      val users = json.fromJson[List[SystemUser]].fold(
        msg => Left(new Exception(s"could not parse system users: $msg")),
        Right(_)
      ).toTry.get

      SystemUsers(users.map(u => (u.username, u)).toMap)
    }.toLayer
  }
}

case class SystemUsers(systemUsers: Map[String, SystemUser] = Map()) {
  def withUser(subject: String, user: SystemUser): SystemUsers = copy(systemUsers = systemUsers + (subject -> user))

  def authenticate(username: String, password: String): Boolean = systemUsers.get(username).exists(_.password == password)
}
