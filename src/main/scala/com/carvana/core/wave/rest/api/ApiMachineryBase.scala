package com.carvana.core.wave.rest.api

import com.carvana.core.{BadClientRequestException, CatchEmAllException, DependencyException, ValidDiscardingRequestException}
import com.carvana.core.logging.{CoreLogContext, coreAnnotation}
import com.carvana.core.zcontext.ZContext
import com.carvana.core.wave.rest.Errors
import sttp.tapir.ztapir._
import zio.logging.{LogAnnotation, Logging}
import zio.telemetry.opentelemetry.Tracing
import zio.zmx.prometheus.PrometheusClient
import zio.{Cause, Has, URIO, ZEnv, ZIO}
import zio.zmx.metrics._
import zio.zmx.metrics.MetricAspect
import com.carvana.core.wave.auth.Authentication
import com.carvana.core.wave.rest.controller.{PolicyController, SecurityController}

trait ApiMachineryBase {

  val baseEndpoint = endpoint
    .errorOut(statusCode and stringBody)
    .mapErrorOut[CatchEmAllException](Errors.decode _)(Errors.encode)

  def logErrors(cause: Cause[_ <: Throwable]): URIO[Logging with Tracing, Unit] = {
    val failure = cause.failureOption
      .orElse(cause.dieOption)
    val className = failure.map(_.getClass.getName).getOrElse("UNKNOWN")

    val failureAnnotation = failure.filter {
      case _: ValidDiscardingRequestException => false
      case _: BadClientRequestException       => false
      case _: DependencyException             => true
      case _                                  => true
    }

    for {
      spanContext <- Tracing.getCurrentSpanContext
      _ <- Logging.locally(
        _.annotate(
          coreAnnotation,
          CoreLogContext(
            "trace-id" -> spanContext.getTraceId,
            "error_class_name" -> className
          )
        ).annotate(LogAnnotation.Throwable, failureAnnotation)
      )(Logging.error(s"http error"))
      _ <- ZIO.unit @@ MetricAspect.count("tapir_request_error_total", "error_class_name" -> className)
    } yield ()
  }

  /**
    * Used to more conveniently define endpoint inputs that put a ResourceName into the URL
    */
  def nameInput(collection: String, description: String = "Unique ID of the resource") = {
    val collectionSlash = collection + "/"
    collection / path[String]("name")
      .map(collectionSlash + _) { x =>
        if (x.startsWith(collectionSlash)) {
          x.drop(collection.length + 1)
        } else {
          // Will only be thrown inside of (broken) tests
          throw new Exception("Bad name: " + x)
        }
      }
      .description(description)
  }
}

object ApiMachineryBase {
  type Env = ZEnv
        with Has[PolicyController.Service]
        with Has[SecurityController.Service]
        with Has[Authentication.Service]
        with Has[PrometheusClient]
        with Tracing
        with ZContext
        with Logging

}
