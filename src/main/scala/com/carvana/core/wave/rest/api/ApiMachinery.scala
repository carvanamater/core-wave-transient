package com.carvana.core.wave.rest.api

import sttp.tapir.ztapir._

trait ApiMachinery extends ApiMachineryBase {

  val v1Endpoint = baseEndpoint.in("api" / "v1")

}
