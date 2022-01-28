package com.carvana.core.wave.jresource

import zio.ZIO
import zio.json.ast.Json

trait ZioSupport {

  implicit class ZioJsonResource[A](resource: JsonResource[A]) {

    def zUpdateUnsafe[R, E](f: Json => ZIO[R, E, Json]): ZIO[R, E, JsonResource[A]] =
      for {
        json2 <- f(resource.json)
      } yield resource.updateJsonUnsafe(json2)
  }
}

object ZioSupport extends ZioSupport
