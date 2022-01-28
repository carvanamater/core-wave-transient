package com.carvana.core.wave.model

import zio.URIO
import zio.clock.Clock
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.time.Instant

object Condition {
  implicit val jsonCodec: JsonCodec[Condition] = DeriveJsonCodec.gen[Condition]

  def replace(a: Condition, b: Condition, now: Instant): Condition = {
    if (compare(a, b)) {
      a
    } else {
      val lastTransitionTime = if (b.status == a.status) a.lastTransitionTime else now
      a.copy(lastUpdateTime = now, lastTransitionTime = lastTransitionTime)
    }
  }

  def compare(a: Condition, b: Condition): Boolean = {
    a.copy(lastUpdateTime = Instant.MIN, lastTransitionTime = Instant.MIN) ==
      b.copy(lastUpdateTime = Instant.MIN, lastTransitionTime = Instant.MIN)
  }
}

case class Condition(
  kind: String = "",
  annotations: Map[String, String] = Map(),
  status: String = "",
  lastUpdateTime: Instant = Instant.MIN,
  lastTransitionTime: Instant = Instant.MIN,
  reason: String = "",
  message: String = ""
)

object Conditions {
  def apply(): Conditions = List()

  def replace(a: Conditions, b: Conditions, immutableKinds: Set[String] = Set()): URIO[Clock, Conditions] = {
    for {
      now <- zio.clock.instant
    } yield {
      val aConditions = a.filter(c => c.kind.nonEmpty && !immutableKinds.contains(c.kind)).map(c => (c.kind, c)).toMap
      val bConditions = b.filter(_.kind.nonEmpty).map(c => (c.kind, c)).toMap
      val immutableConditions = a.filter(c => c.kind.nonEmpty && immutableKinds.contains(c.kind)).map(c => (c.kind, c)).toMap

      // update existing conditions (b in a)
      val existingConditions = bConditions.filter(t => aConditions.contains(t._1))

      // create new conditions (b not in a)
      val newConditions = bConditions.filterNot(t => aConditions.contains(t._1)).map {
        case (k, v) =>
          (k, v.copy(lastUpdateTime = now, lastTransitionTime = now))
      }

      val updatedExistingConditions = existingConditions.map {
        case (kind, condition) =>
          val newCondition = bConditions(kind)
          (kind, Condition.replace(condition, newCondition, now))
      }

      (newConditions ++ updatedExistingConditions ++ immutableConditions).toList.sortBy(_._1).map(_._2)
    }
  }

  def initialize(conditions: Conditions): URIO[Clock, Conditions] = {
    for {
      now <- zio.clock.instant
    } yield conditions.map(
      _.copy(
        lastUpdateTime = now,
        lastTransitionTime = now
      )
    )
  }
}
