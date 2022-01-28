package com.carvana.core.wave.rest

import com.carvana.core.CatchEmAllException
import com.carvana.core.wave.StatusUtil
import sttp.model.StatusCode
import zio.{Cause, URIO}
import zio.logging.Logging

object Errors {
  def decode(tuple: (StatusCode, String)): CatchEmAllException = ???

  def encode(t: CatchEmAllException): (StatusCode, String) = {
    val response = StatusUtil.mapException(t)
    (response.statusCode, response.message)
  }

  def log(c: Cause[Throwable]): URIO[Logging, Unit] = StatusUtil.log("REST")(c)
}
