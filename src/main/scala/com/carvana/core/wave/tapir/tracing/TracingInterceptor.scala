package com.carvana.core.wave.tapir.tracing

import com.carvana.core.wave.tapir.tracing.TracingInterceptor.SpanCustomizer
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator
import io.opentelemetry.api.trace.{SpanKind, StatusCode}
import sttp.model
import sttp.monad.MonadError
import sttp.tapir.Endpoint
import sttp.tapir.metrics.MetricLabels
import sttp.tapir.model.{ServerRequest, ServerResponse}
import sttp.tapir.server.interceptor._
import sttp.tapir.server.interpreter.BodyListener
import zio.telemetry.opentelemetry.Tracing
import zio.{Cause, Has, Promise, RIO, UIO, ZIO}

object TracingInterceptor {

  case class SpanCustomizer(
    onRequest: (ServerRequest, Endpoint[_, _, _, _]) => UIO[Unit] = (_, _) => ZIO.unit,
    onResponse: (ServerResponse[_], Endpoint[_, _, _, _]) => UIO[Unit] = (_, _) => ZIO.unit
  ) {

    def withOnRequest(f: (ServerRequest, Endpoint[_, _, _, _]) => UIO[Unit]): SpanCustomizer = copy(onRequest = f)

    def withOnResponse(f: (ServerResponse[_], Endpoint[_, _, _, _]) => UIO[Unit]): SpanCustomizer = copy(onResponse = f)
  }

  val DEFAULT_METRIC_LABELS: MetricLabels = MetricLabels(
    forRequest = Seq(
      "path_template" -> { case (ep, _) => ep.renderPathTemplate(renderQueryParam = None) },
      "path" -> { case (_, req)         => req.uri.pathSegments.toString },
      "query" -> { case (_, req)        => req.uri.querySegments.mkString("&") },
      "method" -> { case (_, req)       => req.method.method }
    ),
    forResponse = Seq(
      "status" -> {
        case Right(r) => r.code.code.toString
        case Left(_)  => "500"
      }
    )
  )
}

case class TracingInterceptor[R](
  tracing: Tracing.Service,
  labels: MetricLabels = TracingInterceptor.DEFAULT_METRIC_LABELS,
  customizer: SpanCustomizer = SpanCustomizer()
)(
  implicit ev: Has.Union[R, Tracing]
) {
  def interceptor: Interceptor[RIO[R, *]] = requestInterceptor

  def requestInterceptor: RequestInterceptor[RIO[R, *]] = {
    new TracingRequestInterceptor[R](tracing, labels, customizer)
  }
}

class TracingEndpointInterceptor[R](
  tracing: Tracing.Service,
  labels: MetricLabels,
  customizer: SpanCustomizer = SpanCustomizer()
)(
  implicit ev: Has.Union[R, Tracing]
) extends EndpointInterceptor[RIO[R, *]] {

  private val provide = Has(tracing)

  override def apply[B](responder: Responder[RIO[R, *], B], endpointHandler: EndpointHandler[RIO[R, *], B]): EndpointHandler[RIO[R, *], B] = {
    new EndpointHandler[RIO[R, *], B] {
      override def onDecodeSuccess[I](
        ctx: DecodeSuccessContext[RIO[R, *], I]
      )(implicit monad: MonadError[RIO[R, *]], bodyListener: BodyListener[RIO[R, *], B]): RIO[R, ServerResponse[B]] = {
        val endpointName = ctx.endpoint.renderPathTemplate(renderQueryParam = None)
        val reqLabels = labels.forRequestNames.zip(labels.forRequest(ctx.endpoint, ctx.request))

        for {
          promise <- Promise.make[Throwable, ServerResponse[B]]
          headers = ctx.request.headers.map(h => (h.name, h.value)).toMap
          res <- Tracing
            .spanFrom(
              W3CTraceContextPropagator.getInstance(),
              headers,
              TracingUtil.ConsumerGetter,
              endpointName,
              SpanKind.SERVER, {
                case _ => StatusCode.ERROR
              }: PartialFunction[Either[Throwable, sttp.model.StatusCode], StatusCode]
            )(for {
              r <- ZIO.environment[R]
              _ <- ZIO.foreach_(reqLabels)(t => Tracing.setAttribute(t._1, t._2))
              _ <- customizer.onRequest(ctx.request, ctx.endpoint)
              _ <- (for {
                _ <- promise.complete(endpointHandler.onDecodeSuccess(ctx).provide(r))
                res <- promise.await
                resLabels = labels.forResponseNames.zip(labels.forResponse(res)) :+ ("status", res.code.code.toString)
                _ <- ZIO.foreach_(resLabels)(t => Tracing.setAttribute(t._1, t._2))
                _ <- customizer.onResponse(res, ctx.endpoint)
              } yield res)
                .catchAll(err => ZIO.fail(Left(err)))
                .flatMap { response =>
                  if (response.code.isServerError) {
                    ZIO.fail(Right(response.code)).mapErrorCause(_ => Cause.fail(Right(response.code)))
                  } else ZIO.succeed(response.code)
                }
                .unit
                .mapErrorCause(_ => Cause.fail(Right(model.StatusCode.InternalServerError)))
            } yield ())
            .foldM(_ => promise.await, _ => promise.await)
            .provideSome[R](r => ev.union(r, provide))
        } yield res
      }

      override def onDecodeFailure(
        ctx: DecodeFailureContext
      )(implicit monad: MonadError[RIO[R, *]], bodyListener: BodyListener[RIO[R, *], B]): RIO[R, Option[ServerResponse[B]]] = {
        endpointHandler.onDecodeFailure(ctx)
      }
    }
  }
}

class TracingRequestInterceptor[R](
  tracing: Tracing.Service,
  labels: MetricLabels,
  customizer: SpanCustomizer = SpanCustomizer()
)(implicit ev: Has.Union[R, Tracing])
    extends RequestInterceptor[RIO[R, *]] {

  override def apply[B](
    responder: Responder[RIO[R, *], B],
    requestHandler: EndpointInterceptor[RIO[R, *]] => RequestHandler[RIO[R, *], B]
  ): RequestHandler[RIO[R, *], B] = {
    requestHandler(new TracingEndpointInterceptor(tracing, labels, customizer))
  }
}
