package com.carvana.core.wave.jresource

import scala.annotation.Annotation

sealed trait SchemaAnnotation extends Annotation

final case class jsName(name: String) extends Annotation

final case class jsDescription(description: String) extends SchemaAnnotation
final case class jsEnumeration(values: String*) extends SchemaAnnotation
final case class jsMinLength(min: Int) extends SchemaAnnotation
final case class jsMaxLength(max: Int) extends SchemaAnnotation
final case class jsMinItems(min: Int) extends SchemaAnnotation
final case class jsMaxItems(max: Int) extends SchemaAnnotation
final case class jsPattern(pattern: String) extends SchemaAnnotation
final case class jsFormat(format: Format) extends SchemaAnnotation
