package com.carvana.core.wave.webhook

import com.carvana.core.wave.tapir.tracing.TracingUtil
import com.carvana.core.wave.zdiffson.Patch
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator
import sttp.client3.asynchttpclient.zio.{AsyncHttpClientZioBackend, SttpClient}
import sttp.client3.ziojson._
import sttp.client3._
import sttp.client3.internal.Utf8
import sttp.model.{MediaType, StatusCode, Uri}
import zio.{Has, IO, ZIO, ZLayer}
import zio.macros.accessible
import zio.json._
import zio.telemetry.opentelemetry.Tracing

import scala.collection.mutable
import scala.util.Try

@accessible[Webhook.Service]
object Webhook {

  final case class SignedBody(signature: String, body: String)

  abstract class WebhookError(message: String = "", cause: Throwable = None.orNull) extends Exception(message, cause)

  object WebhookError {
    case class Forbidden(message: String = "", cause: Throwable = None.orNull, endpoint: Uri) extends WebhookError(message, cause)

    case class WebhookFailed(message: String = "", cause: Throwable = None.orNull, endpoint: Uri) extends WebhookError(message, cause)

    case class SigningFailed(message: String = "", cause: Throwable = None.orNull, endpoint: Uri) extends WebhookError(message, cause)

    case class PatchFailed(message: String = "", cause: Throwable = None.orNull, endpoint: Uri) extends WebhookError(message, cause)
  }

  val live: ZLayer[SttpClient with Tracing with Has[RequestSigner.Service], Nothing, Has[Webhook.Service]] = {
    (for {
      client <- ZIO.service[SttpClient.Service]
      tracing <- ZIO.service[Tracing.Service]
      requestSigner <- ZIO.service[RequestSigner.Service]
    } yield Live(client, tracing, requestSigner): Service).toLayer
  }

  trait Service {
    def transform[T <: Transform: TransformCompanion](transform: TransformChain[T], endpoint: Uri): IO[WebhookError, TransformChain[T]]

    def transform[T <: Transform: TransformCompanion](transform: TransformChain[T], endpoints: Seq[Uri]): IO[WebhookError, TransformChain[T]]
  }

  case class Live(client: SttpClient.Service, tracing: Tracing.Service, requestSigner: RequestSigner.Service) extends Service {

    private val provide = Has(tracing)

    def signRequest(endpoint: Uri, transform: Transform): IO[WebhookError, SignedBody] = {
      val body = transform.toJson

      requestSigner
        .signRequest(body)
        .mapBoth(
          err => WebhookError.SigningFailed(s"could not sign webhook body", err, endpoint),
          signature => SignedBody(signature = signature, body = body)
        )
    }

    override def transform[T <: Transform: TransformCompanion](transform: TransformChain[T], endpoint: Uri): IO[WebhookError, TransformChain[T]] = {
      for {
        signedBody <- signRequest(endpoint, transform.current)
        headers = mutable.Map[String, String]()
        _ <- Tracing.inject(W3CTraceContextPropagator.getInstance(), headers, TracingUtil.ProducerSetter).provide(provide)
        request = basicRequest
          .post(endpoint)
          .header("x-request-signature", signedBody.signature)
          .headers(headers.toMap)
          .body(StringBody(signedBody.body, "utf-8", MediaType.ApplicationJson))
          .response(asJson[Option[Patch]])
          .mapResponse {
            case Left(err) =>
              err match {
                case err: HttpError[String] =>
                  err.statusCode match {
                    case StatusCode.Forbidden                       => Left(WebhookError.Forbidden(s"operation blocked", err, endpoint))
                    case StatusCode.NotFound | StatusCode.NoContent => Right(None)
                    case _                                          => Left(WebhookError.WebhookFailed(s"webhook failed with status ${err.statusCode}", err, endpoint))
                  }
                case err: DeserializationException[_] =>
                  Left(WebhookError.WebhookFailed(s"webhook failed to deserialize patch body", err, endpoint))
              }
            case Right(right) => Right(right)
          }
        httpResponse <- client.send(request).mapError(err => WebhookError.WebhookFailed(s"webhook failed", err, endpoint))
        response <- ZIO.fromEither(httpResponse.body)
        nextTransformChain <- response.map { patch =>
          ZIO
            .fromTry(transform.tryWithLink(endpoint, patch))
            .mapError(err => WebhookError.PatchFailed("failed to patch request", err, endpoint))
        }.getOrElse(ZIO.succeed(transform))
      } yield nextTransformChain
    }

    override def transform[T <: Transform: TransformCompanion](transform: TransformChain[T], endpoints: Seq[Uri]): IO[WebhookError, TransformChain[T]] = {
      endpoints.foldLeft(ZIO.succeed(transform): IO[WebhookError, TransformChain[T]]) { (acc, ep) =>
        acc.flatMap(t => this.transform(t, ep))
      }
    }
  }
}
