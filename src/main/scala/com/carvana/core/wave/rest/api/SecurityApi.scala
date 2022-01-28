package com.carvana.core.wave.rest.api

import com.carvana.core.wave.rest.controller.SecurityController.PublicKey
import com.carvana.core.wave.rest.controller.SecurityController
import sttp.tapir.generic.auto._
import sttp.tapir.json.zio.jsonBody
import sttp.tapir.ztapir._

trait SecurityApi[Env <: ApiMachineryBase.Env] extends ApiMachinery {

  val v1SecurityEndpoint = v1Endpoint
    .tag("Security")
    .in("security")

  val getPublicKey = v1SecurityEndpoint
    .name("getPublicKey")
    .get
    .in("public-key")
    .out(jsonBody[PublicKey])
    .zServerLogic[Env](_ => SecurityController.getPublicKey)

  val endpoints = List(
    getPublicKey
  )
}

case class DefaultSecurityApi[Env <: ApiMachineryBase.Env]() extends SecurityApi[Env]

object SecurityApi {
  def apply[Env <: ApiMachineryBase.Env]: SecurityApi[Env] = DefaultSecurityApi[Env]()
}
