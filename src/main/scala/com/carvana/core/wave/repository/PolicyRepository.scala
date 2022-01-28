package com.carvana.core.wave.repository

import com.carvana.core.CatchEmAllException
import com.carvana.core.common.{CompactUUID, ResourceName}
import com.carvana.core.validation.{CommonValidators, NameValidators}
import com.carvana.core.wave.model._
import com.carvana.core.wave.repository.Repository.FromPageToken
import com.carvana.core.wave.webhook.{Transform, TransformChain}
import io.getquill.Ord
import zio.json._
import zio.macros.accessible
import zio.{Has, IO, Task, ZIO, ZLayer}
import java.io.Closeable
import java.time.Instant
import java.util.UUID

import io.getquill.context.qzio.ImplicitSyntax.{Implicit, ImplicitSyntaxOps}
import javax.sql.DataSource

@accessible[PolicyRepository.Service]
object PolicyRepository {

  val live: ZLayer[Has[DataSource with Closeable] with Has[EventRepository.Service], Nothing, Has[PolicyRepository.Service]] = {
    (for {
      datasource      <- ZIO.service[DataSource with Closeable]
      eventRepository <- ZIO.service[EventRepository.Service]
    } yield Live(datasource, eventRepository): Service).toLayer
  }

  object Queries extends QuerySupport {
    import ctx._

    lazy val policies = quote(
      querySchema[Policy]("policies")
    )

    def createQ(policy: Policy) = {
      quote(policies.insert(lift(policy)).returningGenerated(o => (o.metadata.id, o.metadata.createTime, o.metadata.updateTime)))
    }

    def create(policy: Policy) = ctx.run(createQ(policy)).map { case (id, createTime, updateTime) =>
      policy.copy(
        metadata = policy.metadata.copy(
          id = id,
          createTime = createTime,
          updateTime = updateTime
        )
      )
    }

    def updateQ(id: UUID, policy: Policy) = {
      quote(
        policies
          .filter(_.metadata.id == lift(id))
          .update(
            _.metadata.labels      -> lift(policy.metadata.labels),
            _.metadata.annotations -> lift(policy.metadata.annotations),
            _.metadata.updateTime  -> infix"now()".as[Instant]
          )
          returning (_.metadata.updateTime)
      )
    }

    def update(id: UUID, policy: Policy) = ctx.run(updateQ(id, policy)).map { updateTime =>
      policy.copy(
        metadata = policy.metadata.copy(updateTime = updateTime)
      )
    }

    def deleteQ(id: UUID) = quote(
      policies
        .filter(_.metadata.id == lift(id))
        .delete
    )

    def delete(id: UUID) = ctx.run(deleteQ(id))

    def getQ(id: UUID) = quote(
      policies
        .filter(_.metadata.id == lift(id))
        .take(1)
    )

    def get(id: UUID) = ctx.run(getQ(id)).flatMap(Queries.singleRow(_)(s"could not find policy with id $id"))

    def getFromQ(id: UUID) = quote(policies.filter(_.metadata.id == lift(id)).map(v => (v.metadata.createTime, v.metadata.id)))

    def getFrom(id: UUID) = ctx.run(getFromQ(id)).map(_.headOption)

    def listQ(filters: Policy.ListRequest.Filters, size: Option[Int], from: FromPageToken) = {
      policies.dynamic
        .filterOpt(filters.owner)((a, u) => a.metadata.owner == u)
        .filterOpt(filters.target)((a, u) => a.target == u)
        .filterOpt(filters.targets)((a, u) => u.contains(a.target))
        .filterOpt(filters.subject.map(List(_, "*")))((a, u) => quote(u.contains(a.subject)))
        .filterOpt(filters.operation)((a, u) => infix"${a.operations} && ARRAY[$u,'*']::TEXT[]".as[Boolean])
        .filterIf(from.filter)(a => {
          quote(a.metadata.createTime < lift(from.fromCreateTime)) ||
            (quote(a.metadata.createTime == lift(from.fromCreateTime)) && infix"${a.metadata.id} < ${lift(from.fromId)}".as[Boolean])
        })
        .takeOpt(size)
        .sortBy(a => (a.metadata.createTime, a.metadata.id))(Ord(Ord.desc, Ord.desc))
    }

    def list(filters: Policy.ListRequest.Filters, size: Option[Int], from: FromPageToken) = ctx.run(listQ(filters, size, from))
  }

  trait Service extends Repository {
    def create(policy: Policy, transformChain: TransformChain[Transform.Post]): IO[CatchEmAllException, Policy]

    def update[R](id: UUID, transformChain: TransformChain[Transform.Put])(
      f: Policy => IO[CatchEmAllException, (Policy, R)]
    ): IO[CatchEmAllException, (Policy, R)]

    def patch[R](id: UUID, transformChain: TransformChain[Transform.Patch])(
      f: Policy => IO[CatchEmAllException, (Policy, R)]
    ): IO[CatchEmAllException, (Policy, R)]
    def delete(id: UUID): IO[CatchEmAllException, Policy]
    def get(id: UUID): IO[CatchEmAllException, Policy]
    def list(list: Policy.ListRequest): IO[CatchEmAllException, Policy.ListResponse]
    def listNoPaging(filters: Policy.ListRequest.Filters): IO[CatchEmAllException, List[Policy]]
  }

  case class Live(ds: DataSource, eventRepository: EventRepository.Service) extends Service { self =>

    import Queries.ctx
    import ctx._

    implicit val env = Implicit(Has(ds))

    override def metadataRepository: MetadataRepository = new MetadataRepository(ds, "policies") {
      override def delete(metadata: Metadata): Task[Unit] = self.delete(metadata.id).unit
    }

    override def create(policy: Policy, transformChain: TransformChain[Transform.Post]): IO[CatchEmAllException, Policy] = {
      val query = for {
        policy2 <- Queries.create(policy)
        _       <- eventRepository.createNoTxn(Event.create("policy", transformChain, policy2))
      } yield policy2

      ctx
        .transaction(query)
        .txnRetry()
        .implicitly
        .mapToDependencyErrors
    }

    override def update[R](id: UUID, transformChain: TransformChain[Transform.Put])(
      f: Policy => IO[CatchEmAllException, (Policy, R)]
    ): IO[CatchEmAllException, (Policy, R)] = {
      val query = for {
        policy      <- Queries.get(id)
        result      <- f(policy)
        (policy2, r) = result
        policy3     <- Queries.update(id, policy2)
        _           <- eventRepository.createNoTxn(Event.put("policy", transformChain, policy, policy3))
      } yield (policy3, r)

      ctx
        .transaction(query)
        .txnRetry()
        .implicitly
        .mapToDependencyErrors
    }

    override def patch[R](id: UUID, transformChain: TransformChain[Transform.Patch])(
      f: Policy => IO[CatchEmAllException, (Policy, R)]
    ): IO[CatchEmAllException, (Policy, R)] = {
      val query = for {
        policy      <- Queries.get(id)
        result      <- f(policy)
        (policy2, r) = result
        policy3     <- Queries.update(id, policy2)
        _           <- eventRepository.createNoTxn(Event.patch("policy", transformChain, policy, policy3))
      } yield (policy3, r)

      ctx
        .transaction(query)
        .txnRetry()
        .implicitly
        .mapToDependencyErrors
    }

    override def delete(id: UUID): IO[CatchEmAllException, Policy] = {
      val query = for {
        policy <- Queries.get(id)
        _      <- Queries.delete(id)
        _      <- eventRepository.createNoTxn(Event.delete("policy", policy))
      } yield policy

      ctx
        .transaction(query)
        .txnRetry()
        .implicitly
        .mapToDependencyErrors
    }

    override def get(id: UUID): IO[CatchEmAllException, Policy] = {
      val query = for {
        policy <- Queries.get(id)
      } yield policy

      query.implicitly.mapToDependencyErrors
    }

    override def list(list: Policy.ListRequest): IO[CatchEmAllException, Policy.ListResponse] = {
      val idZ = list.pageToken.map { t =>
        (for {
          token <- Paging.Token.fromToken(t)
          _     <- token.validateToken(list.filters)
          name  <- CommonValidators
            .validateSafe(NameValidators.validateSimple("name", Policy.Id.prefix, token.name))("invalid name")
        } yield Option(name.uuid)).mapError(err => Repository.InvalidPageToken(err.getMessage, err))
      }.getOrElse(ZIO.none)

      for {
        id       <- idZ
        query     = for {
          from     <- id.map(i => Queries.getFrom(i)).getOrElse(ZIO.none)
          accounts <- Queries.list(list.filters, Option(list.size), FromPageToken(from))
        } yield accounts
        policies <- query.implicitly.mapToDependencyErrors
      } yield {
        Policy.ListResponse(
          policies = policies,
          nextPageToken = policies.lastOption.map(a =>
            Paging
              .Token(
                name = ResourceName.Simple(Policy.Id.prefix, CompactUUID(a.metadata.id)).toString,
                filters = list.filters.toJsonAST.toOption.get
              )
              .toToken
          )
        )
      }
    }

    override def listNoPaging(filters: Policy.ListRequest.Filters): IO[CatchEmAllException, List[Policy]] = {
      val query = Queries.list(filters, None, FromPageToken.NoFilter)

      query
        .txnRetry()
        .implicitly
        .mapToDependencyErrors
    }
  }
}
