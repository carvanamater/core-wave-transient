package com.carvana.core.wave.repository

import com.carvana.core.NotFoundException
import com.carvana.core.wave.model.Metadata
import io.getquill.context.ZioJdbc.DataSourceLayer
import zio.{Has, Task, ZIO}
import zio.stream.ZStream

import java.io.Closeable
import java.sql.SQLException
import java.time.Instant
import javax.sql.DataSource

abstract class MetadataRepository(ds: DataSource, val tableName: String) extends QuerySupport {

  import ctx._

  implicit val env: Has[DataSource] = Has(ds)

  object MetadataQueries {
    lazy val table = dynamicQuerySchema[Metadata](tableName)

    def streamExpiredQ = table.filter(_.expireTime.exists(_ < infix"now()".as[Instant]))

    def streamExpired = ctx.stream(streamExpiredQ)
  }

  def streamExpired: ZStream[Any, Throwable, Metadata] = {
    MetadataQueries.streamExpired
      .refineToOrDie[SQLException]
      .provide(env)
  }

  def deleteExpired: Task[Unit] = streamExpired.foreach(delete(_).catchAll {
    case _: NotFoundException => ZIO.unit
    case err                  => ZIO.fail(err)
  })

  def delete(metadata: Metadata): Task[Unit]

}
