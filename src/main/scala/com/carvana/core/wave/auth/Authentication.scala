package com.carvana.core.wave.auth

import com.carvana.core.common.Credentials
import com.carvana.core.{zcontext, CatchEmAllException, ForbiddenRequestException, SafeException, UnauthorizedRequestException}
import com.carvana.core.wave.model.{Operation, Policy, Target}
import zio.{Has, IO, UIO, ZIO, ZLayer}
import zio.macros.accessible
import com.carvana.core.wave.repository.PolicyRepository
import com.carvana.core.zcontext.ZContext

@accessible[Authentication.Service]
object Authentication {

  val Key: zcontext.Key[AuthenticationContext] = zcontext.Key[AuthenticationContext]("auth.context")

  trait Service {
    def withBasicAuth[R, E, A](basic: Credentials.BasicAuth)(zio: ZIO[R, E, A]): ZIO[R, E, A]

    def withContext[R, E, A](authContext: AuthenticationContext)(zio: ZIO[R, E, A]): ZIO[R, E, A]

    def maybeCurrentContext: UIO[Option[AuthenticationContext]]

    def currentContext: IO[UnauthorizedRequestException, AuthenticationContext]

    def subject: IO[UnauthorizedRequestException, String]

    def checkSystemPermission(owner: String, subject: String, operation: Operation, target: Target): IO[CatchEmAllException, Set[Policy]]

    def checkPermission(owner: String, subject: String, operation: Operation, target: Target): IO[CatchEmAllException, Set[Policy]]

    def checkPermission(owner: String, operation: Operation, target: Target): IO[CatchEmAllException, Set[Policy]]

    def checkPermissions(owner: String, subject: String, operations: Set[Operation], target: Target): IO[CatchEmAllException, Set[Policy]]

    def checkPermissions(owner: String, operations: Set[Operation], target: Target): IO[CatchEmAllException, Set[Policy]]
  }

  val live: ZLayer[ZContext with Has[PolicyRepository.Service] with Has[SystemUsers], Nothing, Has[Service]] = {
    (for {
      repository <- ZIO.service[PolicyRepository.Service]
      context <- ZIO.service[ZContext.Service]
      systemUsers <- ZIO.service[SystemUsers]
    } yield Live(repository, context, systemUsers)).toLayer
  }

  case class Live(repository: PolicyRepository.Service, context: ZContext.Service, systemUsers: SystemUsers) extends Service {

    override def withBasicAuth[R, E, A](basic: Credentials.BasicAuth)(zio: ZIO[R, E, A]): ZIO[R, E, A] = {
      val ctx =
        if (systemUsers.authenticate(basic.username, basic.password)) AuthenticationContext.Authenticated(basic.username) else AuthenticationContext.Anonymous

      context.scoped {
        context.update(Key)(ctx).orDie.unit *> zio
      }.flatten
    }

    override def withContext[R, E, A](authContext: AuthenticationContext)(zio: ZIO[R, E, A]): ZIO[R, E, A] = {
      context.scoped {
        context.update(Key)(authContext).orDie.unit *> zio
      }.flatten
    }

    override def maybeCurrentContext: UIO[Option[AuthenticationContext]] = {
      context.get(Key).map(Option(_)).catchAll(_ => ZIO.none)
    }

    override def currentContext: IO[UnauthorizedRequestException, AuthenticationContext] = {
      context.get(Key).mapError(err => new UnauthorizedRequestException(s"retry with authentication", err) with SafeException)
    }

    override def subject: IO[UnauthorizedRequestException, String] = {
      for {
        ctx <- currentContext
        subject <- ctx match {
          case ctx: AuthenticationContext.WithSubject => ZIO.succeed(ctx.subject)
          case _                                      => ZIO.fail(new UnauthorizedRequestException(s"retry with authentication") with SafeException)
        }
      } yield subject
    }

    override def checkSystemPermission(owner: String, subject: String, operation: Operation, target: Target): IO[CatchEmAllException, Set[Policy]] = {
      val allTargets = target.all

      systemUsers.systemUsers
        .get(owner)
        .map { systemUser =>
          ZIO.succeed {
            systemUser.policies.filter { policy =>
              policy.subject == subject &&
                (policy.operations.contains(operation) || policy.operations.contains(Operation.Wildcard)) &&
              allTargets.contains(policy.target)
            }
          }
        }
        .getOrElse(ZIO.succeed(Set()))
        .flatMap(
          policies =>
            if (policies.isEmpty) {
              ZIO.fail(new ForbiddenRequestException(s"$subject forbidden to ${operation.name.toLowerCase()} $target for owner $owner") with SafeException)
            } else {
              ZIO.succeed(policies.toSet)
            }
        )
    }

    override def checkPermission(owner: String, subject: String, operation: Operation, target: Target): IO[CatchEmAllException, Set[Policy]] = {
      for {
        systemPolicies <- checkSystemPermission(owner, subject, operation, target)
          .catchSome {
            case _: ForbiddenRequestException => ZIO.succeed(Set())
          }
        policies <- repository
          .listNoPaging(
            Policy.ListRequest.Filters(
              owner = Option(owner),
              subject = Option(subject),
              operation = Option(operation),
              targets = Option(target.all)
            )
          )
          .catchSome {
            case _: ForbiddenRequestException => ZIO.succeed(Set())
          }
        allPolicies = systemPolicies ++ policies
        _ <- ZIO.unless(allPolicies.nonEmpty) {
          ZIO.fail(new ForbiddenRequestException(s"$subject forbidden to ${operation.name.toLowerCase()} $target for owner $owner") with SafeException)
        }
      } yield allPolicies
    }

    override def checkPermission(owner: String, operation: Operation, target: Target): IO[CatchEmAllException, Set[Policy]] = {
      for {
        s <- subject
        policies <- checkPermission(owner, s, operation, target)
      } yield policies
    }


    override def checkPermissions(owner: String, subject: String, operations: Set[Operation], target: Target): IO[CatchEmAllException, Set[Policy]] = {
      ZIO.foreach(operations)(checkPermission(owner, subject, _, target)).map(_.flatten)
    }

    override def checkPermissions(owner: String, operations: Set[Operation], target: Target): IO[CatchEmAllException, Set[Policy]] = {
      ZIO.foreach(operations)(checkPermission(owner, _, target)).map(_.flatten)
    }
  }
}
