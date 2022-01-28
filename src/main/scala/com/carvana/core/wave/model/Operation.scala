package com.carvana.core.wave.model

import com.carvana.core.wave.rest.{ModelEnumCompanion, ModelEnumValue}

sealed trait Operation extends ModelEnumValue {
  def name: String

  def annotation: String = s"webhook.$name"
}

object Operation extends ModelEnumCompanion[Operation] {

  override def ALL_VALUES: Set[Operation] = Set(Wildcard, Post, Get, Put, Patch, Delete)

  override def forUnknown(name: String): Option[Operation] = Option(Custom(name))

  case object Unknown extends Operation {
    override def name: String = "unknown"
  }

  case object Wildcard extends Operation {
    override def name: String = "*"
  }

  case object Post extends Operation {
    override def name: String = "post"
  }

  case object Get extends Operation {
    override def name: String = "get"
  }

  case object Put extends Operation {
    override def name: String = "put"
  }

  case object Patch extends Operation {
    override def name: String = "patch"
  }

  case object Delete extends Operation {
    override def name: String = "delete"
  }

  case class Custom(override val name: String) extends Operation
}
