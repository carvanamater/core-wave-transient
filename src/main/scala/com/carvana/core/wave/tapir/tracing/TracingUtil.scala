package com.carvana.core.wave.tapir.tracing

import io.opentelemetry.context.propagation.{TextMapGetter, TextMapSetter}

import scala.jdk.CollectionConverters._
import java.lang
import scala.collection.mutable

trait TracingUtil {

  val ProducerSetter: TextMapSetter[mutable.Map[String, String]] = new TextMapSetter[mutable.Map[String, String]] {

    override def set(carrier: mutable.Map[String, String], key: String, value: String): Unit = {
      carrier += (key -> value)
    }
  }

  val ConsumerGetter: TextMapGetter[Map[String, String]] = new TextMapGetter[Map[String, String]] {
    override def keys(carrier: Map[String, String]): lang.Iterable[String] = carrier.keys.asJava

    override def get(carrier: Map[String, String], key: String): String = carrier.getOrElse(key, None.orNull)
  }
}

object TracingUtil extends TracingUtil
