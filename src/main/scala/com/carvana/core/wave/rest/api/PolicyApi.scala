package com.carvana.core.wave.rest.api

import com.carvana.core.wave.jresource.TapirSupport.jsonResourceBodyUnsafe
import com.carvana.core.wave.model.Policy
import com.carvana.core.wave.rest.controller.PolicyController
import com.carvana.core.wave.zdiffson.Patch
import sttp.tapir.json.zio.jsonBody
import sttp.tapir.ztapir._

trait PolicyApi[Env <: ApiMachineryBase.Env] extends ApiMachinery {

  val v1PolicyEndpoint = v1Endpoint
    .tag("Policies")
    .in("policies")

  val createPolicy = v1PolicyEndpoint
    .name("createPolicy")
    .post
    .in(jsonResourceBodyUnsafe[Policy])
    .out(jsonResourceBodyUnsafe[Policy])
    .zServerLogic[Env](PolicyController.create(_).tapCause(logErrors))

  val getPolicy = v1PolicyEndpoint
    .name("getPolicy")
    .get
    .in(path[String]("id"))
    .out(jsonResourceBodyUnsafe[Policy])
    .zServerLogic[Env](PolicyController.get(_).tapCause(logErrors))

  val replacePolicy = v1PolicyEndpoint
    .name("replacePolicy")
    .put
    .in(path[String]("id"))
    .in(jsonResourceBodyUnsafe[Policy])
    .out(jsonResourceBodyUnsafe[Policy])
    .zServerLogic[Env](tuple => PolicyController.replace(tuple._1, tuple._2).tapCause(logErrors))

  val patchPolicy = v1PolicyEndpoint
    .name("patchPolicy")
    .patch
    .in(path[String]("id"))
    .in(jsonBody[Patch])
    .out(jsonResourceBodyUnsafe[Policy])
    .zServerLogic[Env](tuple => PolicyController.patch(tuple._1, tuple._2).tapCause(logErrors))

  val deletePolicy = v1PolicyEndpoint
    .name("deletePolicy")
    .delete
    .in(path[String]("id"))
    .out(jsonResourceBodyUnsafe[Policy])
    .zServerLogic[Env](PolicyController.delete(_).tapCause(logErrors))

  val endpoints = List(
    createPolicy,
    getPolicy,
    replacePolicy,
    patchPolicy,
    deletePolicy
  )
}

case class DefaultPolicyApi[Env <: ApiMachineryBase.Env]() extends PolicyApi[Env]

object PolicyApi {
  def apply[Env <: ApiMachineryBase.Env]: PolicyApi[Env] = DefaultPolicyApi[Env]()
}
