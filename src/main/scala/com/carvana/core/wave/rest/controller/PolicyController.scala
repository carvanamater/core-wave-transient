package com.carvana.core.wave.rest.controller

import com.carvana.core.validation.{CommonValidators, NameValidators}
import com.carvana.core.{CatchEmAllException, ForbiddenRequestException}
import com.carvana.core.wave.auth.Authentication
import com.carvana.core.wave.jresource.JsonResource
import com.carvana.core.wave.jresource.ZioSupport._
import com.carvana.core.wave.model.{Operation, Policy, Target}
import com.carvana.core.wave.repository.PolicyRepository
import com.carvana.core.wave.zdiffson._
import com.carvana.core.wave.rest.{ResourceUtil, WebhookUtil}
import com.carvana.core.wave.webhook.{Transform, Webhook}
import zio.clock.Clock
import zio.macros.accessible
import zio.{Has, IO, ZIO, ZLayer}

@accessible[PolicyController.Service]
object PolicyController {

  val live
    : ZLayer[Has[PolicyRepository.Service] with Has[Webhook.Service] with Has[Authentication.Service] with Clock, Nothing, Has[PolicyController.Service]] = {
    (for {
      authentication <- ZIO.service[Authentication.Service]
      policyRepository <- ZIO.service[PolicyRepository.Service]
      webhook <- ZIO.service[Webhook.Service]
      clock <- ZIO.service[Clock.Service]
    } yield Live(authentication, policyRepository, webhook, clock)).toLayer
  }

  trait Service {
    def create(request: JsonResource[Policy]): IO[CatchEmAllException, JsonResource[Policy]]
    def get(name: String): IO[CatchEmAllException, JsonResource[Policy]]
    def replace(name: String, request: JsonResource[Policy]): IO[CatchEmAllException, JsonResource[Policy]]
    def patch(name: String, patch: Patch): IO[CatchEmAllException, JsonResource[Policy]]
    def delete(name: String): IO[CatchEmAllException, JsonResource[Policy]]
    def list(list: Policy.ListRequest): IO[CatchEmAllException, JsonResource[Policy.ListResponse]]
  }

  case class Live(authentication: Authentication.Service, repository: PolicyRepository.Service, webhook: Webhook.Service, clock: Clock.Service)
      extends Service
      with Controller {

    private val provide = Has(clock) ++ Has(webhook) ++ Has(authentication)

    override def create(request: JsonResource[Policy]): IO[CatchEmAllException, JsonResource[Policy]] = {
      request.zUpdateUnsafe { json =>
        for {
          subject <- authentication.subject
          requestOwner <- getOwnerForRequest(request)
          can <- authentication.checkPermission(requestOwner, subject, Operation.Post, Target.Root / "policies").either
          canGrant <- authentication.checkPermission(subject, subject, Policy.Grant, Target.Root / "policies").either
          _ <- ZIO.fromEither(can).when(can.isLeft && canGrant.isLeft)
          transformedPolicy <- WebhookUtil.transform(Transform.post("policy", json), json)
          createPolicy <- decode[Policy](transformedPolicy.current.request)
          _ <- if (canGrant.isRight) {
            authentication.checkPermissions(requestOwner, subject, createPolicy.operations, createPolicy.target).catchSome {
              case _: ForbiddenRequestException =>
                authentication.checkPermissions(subject, subject, createPolicy.operations, createPolicy.target)
            }
          } else {
            authentication.checkPermissions(requestOwner, subject, createPolicy.operations, createPolicy.target)
          }
          createPolicy2 <- Policy.initialize(createPolicy)
          createPolicy3 <- ResourceUtil.createAuthenticated(createPolicy2, owner = Option(requestOwner))
          createdPolicy <- repository.create(createPolicy3, transformedPolicy)
          createdPolicyJson <- encode(createdPolicy)
        } yield createdPolicyJson
      }.provide(provide)
    }

    override def get(name: String): IO[CatchEmAllException, JsonResource[Policy]] = {
      (for {
        simpleName <- CommonValidators.validateSafe(NameValidators.validateSimple("name", Policy.Id.prefix, name))("invalid name")
        policy <- repository.get(simpleName.uuid)
        _ <- authentication.checkPermission(policy.metadata.owner, Operation.Get, Target.Root / "policies" / name)
        policyJson <- encode(policy, "policy")
      } yield JsonResource.makeUnsafe[Policy](policyJson)).wrapNotFound
    }

    override def replace(name: String, request: JsonResource[Policy]): IO[CatchEmAllException, JsonResource[Policy]] = {
      request.zUpdateUnsafe { json =>
        (for {
          simpleName <- CommonValidators.validateSafe(NameValidators.validateSimple("name", Policy.Id.prefix, name))("invalid name")
          policy <- repository.get(simpleName.uuid)
          transformedPolicy <- WebhookUtil.transform(Transform.put("policy", name, json), policy).provide(provide)
          updatePolicy <- decode[Policy](transformedPolicy.current.request)
          _ <- authentication.checkPermission(updatePolicy.metadata.owner, Operation.Put, Target.Root / "policies" / name)
          result <- repository.update(simpleName.uuid, transformedPolicy) { policy =>
            (for {
              updated <- Policy.replace(policy, updatePolicy)
              json <- encode[Policy](updated)
            } yield (updated, json)).provide(provide)
          }
          (_, updatedPolicyJson) = result
        } yield updatedPolicyJson).wrapNotFound
      }
    }

    override def patch(name: String, patch: Patch): IO[CatchEmAllException, JsonResource[Policy]] = {
      (for {
        simpleName <- CommonValidators.validateSafe(NameValidators.validateSimple("name", Policy.Id.prefix, name))("invalid name")
        policy <- repository.get(simpleName.uuid)
        transformedPatch <- WebhookUtil.transform(Transform.patch("policy", name, patch.toJson), policy).provide(provide)
        updatePatch <- decode[Patch](transformedPatch.current.request)
        result <- repository.patch(
          simpleName.uuid,
          transformedPatch
        ) { policy =>
          (for {
            policyJson <- encode(policy)
            updatePolicyJson <- ZIO
              .fromTry(updatePatch(policyJson))
              .mapError(err => InvalidResourceException(err.getMessage, err))
            updatePolicy <- decode[Policy](updatePolicyJson)
            _ <- authentication.checkPermission(updatePolicy.metadata.owner, Operation.Patch, Target.Root / "policies" / name)
            updateMessage2 <- ResourceUtil.updateAuthenticated(updatePolicy)
            updated <- Policy.replace(policy, updateMessage2)
            json <- encode(updated)
          } yield (updated, json)).provide(provide)
        }
        (_, updatedPolicyJson) = result
      } yield JsonResource.makeUnsafe[Policy](updatedPolicyJson)).wrapNotFound
    }

    override def delete(name: String): IO[CatchEmAllException, JsonResource[Policy]] = {
      (for {
        simpleName <- CommonValidators.validateSafe(NameValidators.validateSimple("name", Policy.Id.prefix, name))("invalid name")
        policy <- repository.get(simpleName.uuid)
        policyJson <- encode(policy, "policy")
        _ <- WebhookUtil.transform(Transform.delete("policy", simpleName.toString, policyJson), policy).provide(provide)
        _ <- authentication.checkPermission(policy.metadata.owner, Operation.Delete, Target.Root / "policies" / name)
        deletedPolicy <- repository.delete(simpleName.uuid)
        deletedPolicyJson <- encode(deletedPolicy, "policy")
      } yield JsonResource.makeUnsafe[Policy](deletedPolicyJson)).wrapNotFound
    }

    override def list(list: Policy.ListRequest): IO[CatchEmAllException, JsonResource[Policy.ListResponse]] = {
      for {
        owner <- list.filters.owner.map(ZIO.succeed(_)).getOrElse(authentication.subject)
        policies <- repository.list(list)
        _ <- authentication.checkPermission(owner, Operation.Get, Target.Root / "accounts")
        policiesJson <- encode(policies, "policies")
      } yield JsonResource.makeUnsafe[Policy.ListResponse](policiesJson)
    }
  }
}
