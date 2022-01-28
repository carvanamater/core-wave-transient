package com.carvana.core.wave.rest

import com.carvana.core.{CatchEmAllException, ForbiddenRequestException, SafeException, UnknownException}
import com.carvana.core.wave.model
import com.carvana.core.wave.rest.WebhookUtil.WebhookEndpoint
import com.carvana.core.wave.zdiffson.JsonyZioJson
import com.carvana.core.wave.webhook.Webhook.WebhookError
import com.carvana.core.wave.webhook.{Transform, TransformChain, TransformCompanion, Webhook}
import diffson.jsonpointer.Pointer
import sttp.model.Uri
import zio.json.ast
import zio.{Has, ZIO}

import scala.language.implicitConversions
import scala.util.Try

trait WebhookUtil {

  def transform[T <: Transform : TransformCompanion](transform: TransformChain[T], endpoints: WebhookEndpoint*): ZIO[Has[Webhook.Service], CatchEmAllException, TransformChain[T]] = {
    val uris = endpoints
      .flatMap(_.endpoint(transform.original.operation, transform.original.kind))

    for {
      transformed <- Webhook.transform(transform, uris).mapError {
        case err: WebhookError.Forbidden     => new ForbiddenRequestException(s"webhook request forbidden by ${err.endpoint}") with SafeException
        case err: WebhookError.WebhookFailed => new UnknownException(s"webhook request failed at ${err.endpoint}") with SafeException
        case err: WebhookError.PatchFailed   => new UnknownException(s"webhook patch failed at ${err.endpoint}") with SafeException
      }
    } yield transformed
  }
}

object WebhookUtil extends WebhookUtil {

  sealed trait WebhookEndpoint {
    def endpoint(operation: String, resource: String): Seq[Uri]
  }

  object WebhookEndpoint {

    implicit def apply(resource: model.Resource): Resource = Resource(resource)

    implicit def apply(json: ast.Json): Json = Json(json)

    implicit def apply(string: Predef.String): String = String(string)

    case class OrderedEndpoint(endpoint: Uri, order: Int)

    case class Resource(resource: model.Resource) extends WebhookEndpoint {

      override def endpoint(operation: Predef.String, resource: Predef.String): Seq[Uri] = {
        Seq(
          s"webhook.*.$resource",
          s"webhook.$operation.$resource"
        ).flatMap { annotation =>
          this.resource.metadata.annotations.get(annotation).flatMap(Uri.parse(_).toOption)
        }
      }
    }

    case class Json(json: ast.Json) extends WebhookEndpoint {

      override def endpoint(operation: Predef.String, resource: Predef.String): Seq[Uri] = {
        Seq(
          Pointer.Root / "metadata" / "annotations" / s"webhook.*.$resource",
          Pointer.Root / "metadata" / "annotations" / s"webhook.$operation.$resource",
        ).flatMap(_.evaluate[Try, ast.Json](json).toOption).flatMap {
          case ast.Json.Str(value) => Uri.parse(value).toOption
          case _ => None
        }
      }
    }

    case class String(string: Predef.String) extends WebhookEndpoint {
      override def endpoint(operation: Predef.String, resource: Predef.String): Seq[Uri] = Uri.parse(string).toOption.toSeq
    }
  }
}
