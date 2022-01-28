package com.carvana.core.wave.auth

import com.carvana.core.validation.WireValidators
import sttp.monad.MonadError
import sttp.tapir.model.ServerResponse
import sttp.tapir.server.interceptor.{DecodeFailureContext, DecodeSuccessContext, EndpointHandler, EndpointInterceptor, Interceptor, RequestHandler, RequestInterceptor, Responder}
import sttp.tapir.server.interpreter.BodyListener
import zio.RIO
import zio.prelude.Validation

case class AuthenticationInterceptor[R](auth: Authentication.Service) {
  def interceptor: Interceptor[RIO[R, *]] = requestInterceptor

  def requestInterceptor: RequestInterceptor[RIO[R, *]] = new AuthenticationRequestInterceptor[R](auth)
}

class AuthenticationRequestInterceptor[R](auth: Authentication.Service) extends RequestInterceptor[RIO[R, *]] {

  override def apply[B](
    responder: Responder[RIO[R, *], B],
    requestHandler: EndpointInterceptor[RIO[R, *]] => RequestHandler[RIO[R, *], B]
  ): RequestHandler[RIO[R, *], B] = {
    requestHandler(new AuthenticationEndpointInterceptor[R](auth))
  }
}

class AuthenticationEndpointInterceptor[R](auth: Authentication.Service) extends EndpointInterceptor[RIO[R, *]] {

  override def apply[B](responder: Responder[RIO[R, *], B], endpointHandler: EndpointHandler[RIO[R, *], B]): EndpointHandler[RIO[R, *], B] = {
    new EndpointHandler[RIO[R, *], B] {
      override def onDecodeSuccess[I](
        ctx: DecodeSuccessContext[RIO[R, *], I]
      )(implicit monad: MonadError[RIO[R, *]], bodyListener: BodyListener[RIO[R, *], B]): RIO[R, ServerResponse[B]] = {
        ctx.request
          .header("authorization")
          .map(WireValidators.validateBasicAuthCredentials("headers.authorization", _))
          .flatMap {
            case Validation.Success(_, basic) => Option(basic)
            case _                         => None
          }
          .map { basic =>
              auth.withBasicAuth(basic)(endpointHandler.onDecodeSuccess(ctx))
          }
          .getOrElse {
            val isAuthenticated = ctx.request.header("x-auth-is-authenticated").flatMap(_.toBooleanOption).getOrElse(false)
            val subject = ctx.request.header("x-auth-subject")
            val authContext = AuthenticationContext(isAuthenticated, subject)

            auth.withContext(authContext)(endpointHandler.onDecodeSuccess(ctx))
          }
      }

      override def onDecodeFailure(
        ctx: DecodeFailureContext
      )(implicit monad: MonadError[RIO[R, *]], bodyListener: BodyListener[RIO[R, *], B]): RIO[R, Option[ServerResponse[B]]] = {
        endpointHandler.onDecodeFailure(ctx)
      }
    }
  }
}
