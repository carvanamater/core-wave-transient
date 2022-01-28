package com.carvana.core.wave.repository

import com.carvana.core.BadClientRequestException
import com.carvana.core.wave.model.Common
import io.getquill.context.ZioJdbc.QDataSource
import zio.clock.Clock
import zio.duration._
import zio.logging.Logging
import zio.{Has, ZIO, ZLayer}

import java.io.Closeable
import java.time.Instant
import java.util.UUID
import javax.sql.DataSource

trait Repository {
  def metadataRepository: MetadataRepository
}

object Repository {

  val DefaultExpireJobDelay: Duration = 1.minute

  case class InvalidPageToken(message: String = "", cause: Throwable = None.orNull) extends BadClientRequestException(message, cause)

  object FromPageToken {

    val NoFilter: FromPageToken = FromPageToken(filter = false, Instant.MIN, Common.UUID_ZERO)

    def apply(t: Option[(Instant, UUID)]): FromPageToken = {
      t.map(v => FromPageToken(filter = true, v._1, v._2))
    }.getOrElse(FromPageToken.NoFilter)
  }

  case class FromPageToken(filter: Boolean, fromCreateTime: Instant, fromId: UUID)

  lazy val liveQDataSource: ZLayer[Any, Nothing, Has[DataSource with Closeable]] = {
    QDataSource.fromPrefix("com.carvana.core.db.hikari")
  }.orDie

  def expireJob(delay: Duration = DefaultExpireJobDelay)(repositories: MetadataRepository*): ZIO[Clock with Logging, Nothing, Unit] = {
    ZIO
      .foreach_(repositories) { repository =>
        Logging.info(s"cleaning up $repository") *>
          repository.deleteExpired
            .tapCause(cause => Logging.error(s"error running cleanup job for ${repository.tableName}", cause))
            .ignore
      }
      .delay(delay)
      .forever
  }
}
