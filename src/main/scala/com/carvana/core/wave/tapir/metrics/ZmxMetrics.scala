package com.carvana.core.wave.tapir.metrics

import sttp.tapir.Endpoint
import sttp.tapir.metrics.{EndpointMetric, Metric, MetricLabels}
import sttp.tapir.model.{ServerRequest, ServerResponse}
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.interceptor.metrics.MetricsRequestInterceptor
import sttp.tapir.ztapir._
import zio.clock.Clock
import zio.zmx.metrics._
import zio.zmx.prometheus.PrometheusClient
import zio.{Chunk, RIO, ZIO}
import zio.duration._

import java.util.concurrent.TimeUnit

case class ZmxMetrics[R](
  namespace: String,
  prometheusClient: PrometheusClient,
  metrics: List[Metric[RIO[R, *], _]] = List.empty[Metric[RIO[R, *], _]]
) {
  import ZmxMetrics._

  lazy val metricsEndpoint: ServerEndpoint[Unit, Unit, String, Any, RIO[R, *]] = ServerEndpoint(
    endpoint.get.in("metrics").out(plainBody[String]),
    _ => _ => prometheusClient.snapshot.map(_.value).map(v => Right(v): Either[Unit, String])
  )

  def withRequestsTotal(namespace: String, labels: MetricLabels = MetricLabels.Default): ZmxMetrics[R] =
    copy(metrics = metrics :+ requestsTotal(namespace, labels))

  def withRequestsActive(namespace: String, labels: MetricLabels = MetricLabels.Default): ZmxMetrics[R] =
    copy(metrics = metrics :+ requestsActive(namespace, labels))

  def withResponsesTotal(namespace: String, labels: MetricLabels = MetricLabels.Default): ZmxMetrics[R] =
    copy(metrics = metrics :+ responsesTotal(namespace, labels))

  def withResponsesDuration(
    namespace: String,
    labels: MetricLabels = MetricLabels.Default,
    clock: Clock.Service,
    boundaries: Chunk[Double] = DEFAULT_BOUNDARIES
  ): ZmxMetrics[R] =
    copy(metrics = metrics :+ responsesDuration(namespace, labels, clock, boundaries))

  def withCustom(m: Metric[RIO[R, *], _]): ZmxMetrics[R] = copy(metrics = metrics :+ m)

  def metricsInterceptor(ignoreEndpoints: Seq[Endpoint[_, _, _, _]] = Seq.empty): MetricsRequestInterceptor[RIO[R, *]] =
    new MetricsRequestInterceptor[RIO[R, *]](metrics, ignoreEndpoints :+ metricsEndpoint.endpoint)
}

object ZmxMetrics {

  val DEFAULT_BOUNDARIES: Chunk[Double] = Chunk(0.01, 0.025, 0.05, 0.075, 0.1, 0.5, 1.0, 2.0, 5.0, 10.0)

  private def tagsForEndpointRequest(ep: Endpoint[_, _, _, _], req: ServerRequest, labels: MetricLabels): Seq[(String, String)] = {
    labels.forRequestNames.zip(labels.forRequest(ep, req))
  }

  private def tagsForResponse(res: ServerResponse[_], labels: MetricLabels): Seq[(String, String)] = {
    labels.forResponseNames.zip(labels.forResponse(res))
  }

  private def tagsForResponse(err: Throwable, labels: MetricLabels): Seq[(String, String)] = {
    labels.forResponseNames.zip(labels.forResponse(err))
  }

  def withDefaultMetrics[R](
    prometheusClient: PrometheusClient,
    clock: Clock.Service,
    namespace: String = "tapir_",
    labels: MetricLabels = MetricLabels.Default,
    boundaries: Chunk[Double] = DEFAULT_BOUNDARIES
  ): ZmxMetrics[R] =
    ZmxMetrics(
      namespace,
      prometheusClient,
      List(
        requestsTotal(namespace, labels),
        requestsActive(namespace, labels),
        responsesTotal(namespace, labels),
        responsesDuration(namespace, labels, clock, boundaries)
      )
    )

  def requestsTotal[R](namespace: String, labels: MetricLabels): Metric[RIO[R, *], Any] =
    Metric(
      (),
      onRequest = { (req, _, m) =>
        m.unit {
          EndpointMetric().onEndpointRequest { ep =>
            ZIO.unit @@ MetricAspect.count(s"${namespace}requests_total", tagsForEndpointRequest(ep, req, labels): _*)
          }
        }
      }
    )

  def requestsActive[R](namespace: String, labels: MetricLabels): Metric[RIO[R, *], Unit] =
    Metric(
      (),
      onRequest = { (req, gauge, m) =>
        m.unit {
          EndpointMetric[RIO[R, *]]().onEndpointRequest { ep =>
            (ZIO.succeed(1.0) @@ MetricAspect.adjustGauge(s"${namespace}requests_active", tagsForEndpointRequest(ep, req, labels): _*)).unit
          }.onResponse { (ep, _) =>
            (ZIO.succeed(-1.0) @@ MetricAspect.adjustGauge(s"${namespace}requests_active", tagsForEndpointRequest(ep, req, labels): _*)).unit
          }.onException { (ep, _) =>
            (ZIO.succeed(-1.0) @@ MetricAspect.adjustGauge(s"${namespace}requests_active", tagsForEndpointRequest(ep, req, labels): _*)).unit
          }
        }
      }
    )

  def responsesTotal[R](namespace: String, labels: MetricLabels): Metric[RIO[R, *], Unit] =
    Metric[RIO[R, *], Unit](
      (),
      onRequest = { (req, _, m) =>
        m.unit {
          EndpointMetric[RIO[R, *]]().onResponse { (ep, res) =>
            (ZIO.unit @@ MetricAspect.count(s"${namespace}responses_total", tagsForEndpointRequest(ep, req, labels) ++ tagsForResponse(res, labels): _*)).unit
          }.onException { (ep, ex) =>
            (ZIO.unit @@ MetricAspect.count(s"${namespace}responses_total", tagsForEndpointRequest(ep, req, labels) ++ tagsForResponse(ex, labels): _*)).unit
          }
        }
      }
    )

  def responsesDuration[R](
    namespace: String,
    labels: MetricLabels,
    clock: Clock.Service,
    boundaries: Chunk[Double]
  ): Metric[RIO[R, *], Unit] =
    Metric[RIO[R, *], Unit](
      (),
      onRequest = { (req, _, m) =>
        for {
          requestStart <- clock.nanoTime
        } yield {
          EndpointMetric[RIO[R, *]]().onResponse { (ep, res) =>
            for {
              requestEnd <- clock.nanoTime
              durationSeconds = Duration(requestEnd - requestStart, TimeUnit.NANOSECONDS).toMillis.toDouble / 1000.0
              _ <- ZIO.succeed(durationSeconds) @@ MetricAspect.observeHistogram(
                s"${namespace}responses_duration",
                boundaries,
                tagsForEndpointRequest(ep, req, labels) ++ tagsForResponse(res, labels): _*
              )
            } yield ()
          }.onException { (ep, ex) =>
            for {
              requestEnd <- clock.nanoTime
              durationSeconds = Duration(requestEnd - requestStart, TimeUnit.NANOSECONDS).toMillis.toDouble / 1000.0
              _ <- ZIO.succeed(durationSeconds) @@ MetricAspect.observeHistogram(
                s"${namespace}responses_duration",
                boundaries,
                tagsForEndpointRequest(ep, req, labels) ++ tagsForResponse(ex, labels): _*
              )
            } yield ()
          }
        }
      }
    )
}
