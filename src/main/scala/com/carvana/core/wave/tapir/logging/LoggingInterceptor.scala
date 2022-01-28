package com.carvana.core.wave.tapir.logging

import com.carvana.core.logging._
import com.carvana.core.wave.tapir.logging.LoggingInterceptor.Customizer
import sttp.monad.MonadError
import sttp.tapir.model.ServerResponse
import sttp.tapir.server.interceptor._
import sttp.tapir.server.interpreter.BodyListener
import zio.clock.Clock
import zio.duration._
import zio.logging.Logger
import zio.{RIO, UIO}

import java.util.concurrent.TimeUnit

object LoggingInterceptor {

  case class Customizer[R](onLog: RIO[R, Unit] => RIO[R, Unit] = (v: RIO[R, Unit]) => v) {
    def withOnLog(f: RIO[R, Unit] => RIO[R, Unit]): Customizer[R] = copy(onLog = f)
  }
}

case class LoggingInterceptor[R](logger: Logger[String], clock: Clock.Service, customizer: Customizer[R] = Customizer()) {
  def interceptor: Interceptor[RIO[R, *]] = requestInterceptor

  def requestInterceptor: RequestInterceptor[RIO[R, *]] = new LoggingRequestInterceptor[R](logger, clock, customizer)
}

class LoggingRequestInterceptor[R](logger: Logger[String], clock: Clock.Service, customizer: Customizer[R])
    extends RequestInterceptor[RIO[R, *]] {

  override def apply[B](
    responder: Responder[RIO[R, *], B],
    requestHandler: EndpointInterceptor[RIO[R, *]] => RequestHandler[RIO[R, *], B]
  ): RequestHandler[RIO[R, *], B] = {
    RequestHandler.from { (request, monad) =>
      implicit val m: MonadError[RIO[R, *]] = monad

      for {
        requestStart <- clock.nanoTime
        res <- requestHandler(new LoggingEndpointInterceptor[R](logger, clock, requestStart, customizer))(request)
      } yield res
    }
  }
}

class LoggingEndpointInterceptor[R](logger: Logger[String], clock: Clock.Service, requestStart: Long, customizer: Customizer[R]) extends EndpointInterceptor[RIO[R, *]] {

  override def apply[B](responder: Responder[RIO[R, *], B], endpointHandler: EndpointHandler[RIO[R, *], B]): EndpointHandler[RIO[R, *], B] = {
    new EndpointHandler[RIO[R, *], B] {
      override def onDecodeSuccess[I](
        ctx: DecodeSuccessContext[RIO[R, *], I]
      )(implicit monad: MonadError[RIO[R, *]], bodyListener: BodyListener[RIO[R, *], B]): RIO[R, ServerResponse[B]] = {
        val path = ctx.endpoint.renderPathTemplate(renderQueryParam = None)
        val method = ctx.endpoint.httpMethod.map(_.method).getOrElse("UNKNOWN")

        for {
          res <- endpointHandler.onDecodeSuccess(ctx)
          requestEnd <- clock.nanoTime
          status = res.code.code.toString
          _ <- customizer.onLog(logResult(logger, path = path, method = method, status = status, requestStart = requestStart, requestEnd = requestEnd))
          //          res2 <- res.body.map { body =>
//            body.onComplete { t: Try[Unit] =>
//              (t match {
//                case Success(_) =>
//                  logResult(logger, path = path, method = method, status = status, requestStart = requestStart, requestEnd = requestEnd, err = None)
//                case Failure(err) =>
//                  logResult(logger, path = path, method = method, status = status, requestStart = requestStart, requestEnd = requestEnd, err = Option(err))
//              }).unit: RIO[R, Unit]
//            }.map(b => res.copy(body = Option(b)))
//          }.getOrElse {
//            logResult(logger, path = path, method = method, status = status, requestStart = requestStart, requestEnd = requestEnd, err = None).as(res)
//          }
        } yield res
      }

      override def onDecodeFailure(
        ctx: DecodeFailureContext
      )(implicit monad: MonadError[RIO[R, *]], bodyListener: BodyListener[RIO[R, *], B]): RIO[R, Option[ServerResponse[B]]] = {
        endpointHandler.onDecodeFailure(ctx)
      }

      def logResult(
        logger: Logger[String],
        path: String,
        method: String,
        status: String,
        requestStart: Long,
        requestEnd: Long
      ): UIO[Unit] = {
        val durationMillis = Duration(requestEnd - requestStart, TimeUnit.NANOSECONDS).toMillis.toString

        logger.locally(
          _.annotate(
            coreAnnotation,
            CoreLogContext(
              "path" -> path,
              "method" -> method,
              "status" -> status,
              "duration_ms" -> durationMillis
            )
          )
        ) {
          logger.info(s"$method $path in ${durationMillis}ms")
        }
      }
    }
  }
}
