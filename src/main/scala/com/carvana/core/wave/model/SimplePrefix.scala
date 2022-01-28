package com.carvana.core.wave.model

import com.carvana.core.common.{CompactUUID, ResourceName}

import java.util.UUID

trait SimplePrefix[R] {
  def prefix: String

  def name(uuid: UUID): String = ResourceName.Simple(prefix, CompactUUID(uuid)).toString
}

object SimplePrefix {
  def generic[R](_prefix: String): SimplePrefix[R] = new SimplePrefix[R] {
    override def prefix: String = _prefix
  }
}
