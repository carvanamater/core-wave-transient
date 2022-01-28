package com.carvana.core.wave

import com.carvana.core.validation.ValidationException
import com.carvana.core._
import sttp.model.StatusCode
import zio.{Cause, URIO}
import zio.logging.{LogAnnotation, Logging}

/**
 * Represents information for generating a response when an error has occured
 *
 * @param statusCode sttp StatusCode that can be used in a response
 * @param enumString enumeration string that can be used in e.g. GraphQL error extensions
 * @param message longer message, suitable for response body
 */
case class StatusResponse(statusCode: StatusCode, enumString: String, message: String)

// Based on com.carvana.core.proto.StatusUtil
object StatusUtil {

  private def throwableToStatusResponse(err: CatchEmAllException) = err match {
    case _: ThrottledRequestException    =>
      StatusResponse(StatusCode.TooManyRequests, "TooManyRequests", "request was throttled, retry later")
    case _: NotFoundRequestException     =>
      StatusResponse (StatusCode.NotFound, "NotFound", "resource not found")
    case _: FailedPreconditionException  =>
      StatusResponse (StatusCode.PreconditionFailed, "PreconditionFailed", "failed precondition")
    case _: ConflictRequestException            =>
      StatusResponse(StatusCode.Conflict, "Conflict", "already exists")
    case _: ValidationException          =>
      StatusResponse(StatusCode.BadRequest, "BadRequest", "invalid request, check values before retrying")
    case _: UnauthorizedRequestException =>
      StatusResponse(StatusCode.Unauthorized, "Unauthorized", "unauthenticated, authenticate before retrying")
    case _: ForbiddenRequestException    =>
      StatusResponse(StatusCode.Forbidden, "Forbidden", "permission denied")
    case _: ValidDiscardingRequestException =>
      StatusResponse(StatusCode.BadRequest, "BadRequest", "bad request")
    case _: BadClientRequestException    =>
      StatusResponse(StatusCode.BadRequest, "BadRequest", "bad request")
    case _: BadRequestException    =>
      StatusResponse(StatusCode.InternalServerError, "InternalServerError", "downstream failure")
    case _: DependencyException          =>
      StatusResponse(StatusCode.InternalServerError, "InternalServerError", "downstream failure")
  }

  def mapException(err: Throwable): StatusResponse = {
    val (description, underlying) = err match {
      case err: SafeException => (Option(err.getMessage), err)
      case err                => (None, err)
    }

    val response = underlying match {
      case err: CatchEmAllException =>
        throwableToStatusResponse(err)
      case _                        =>
        StatusResponse(StatusCode.InternalServerError, "InternalServerError", "internal error")
    }

    val validationExtraDesc = ValidationException.collapseSafe(err)().map(v => s"\n${v.prettyPrint}").getOrElse("")
    val finalDescription = description.getOrElse(response.message) + validationExtraDesc
    response.copy(message = finalDescription)
  }

  def log(endpoint: String)(c: Cause[Throwable]): URIO[Logging, Unit] = {
    Logging.locally(LogAnnotation.Cause(Some(c))) {
      c.failureOption match {
        case Some(_: SafeException) =>
          Logging.info(s"Error handling $endpoint request")
        case _ =>
          Logging.error(s"Error handling $endpoint request")
      }
    }
  }
}
