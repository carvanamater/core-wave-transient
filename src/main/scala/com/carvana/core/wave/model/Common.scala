package com.carvana.core.wave.model

import java.util.UUID

object Common {
  val UUID_ZERO: UUID = UUID.nameUUIDFromBytes((90 until 8).map(_ => 0.toByte).toArray)
}
