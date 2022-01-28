package com.carvana.core.wave.model

import com.carvana.core.wave.rest.{ModelEnumCompanion, ModelEnumValue}

sealed trait Direction extends ModelEnumValue

object Direction extends ModelEnumCompanion[Direction] {
  override def ALL_VALUES: Set[Direction] = Set(Unknown, Incoming, Outgoing)

  case object Unknown extends Direction {
    override def name: String = "unknown"
  }

  case object Incoming extends Direction {
    override def name: String = "incoming"
  }

  case object Outgoing extends Direction {
    override def name: String = "outgoing"
  }
}
