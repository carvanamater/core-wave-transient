package com.carvana.core.wave.rest

import com.carvana.core.common.{CompactUUID, ResourceName}

import scala.util.matching.Regex

trait ResourceNameUtil {
  val PREFIX_REGEX: Regex = "([A-Z]{2,4})([a-f0-9]{32})".r

  def patternForPrefix(prefix: String): String = s"""$prefix([a-f0-9]{32})"""

  def parseSimpleName(name: String): Either[String, ResourceName.Simple] = {
    name match {
      case PREFIX_REGEX(prefix, compactUuid) => Right(ResourceName.simpleFromCompact(prefix, CompactUUID.fromString(compactUuid)))
      case _                                 => Left("invalid simple resource name")
    }
  }

}

object ResourceNameUtil extends ResourceNameUtil
