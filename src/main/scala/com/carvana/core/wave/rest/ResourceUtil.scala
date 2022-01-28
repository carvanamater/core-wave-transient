package com.carvana.core.wave.rest

import com.carvana.core.CatchEmAllException
import com.carvana.core.wave.model.{Resource, ResourceCompanion}
import zio.{Has, ZIO}
import com.carvana.core.wave.auth.Authentication

trait ResourceUtil {

  def createAuthenticated[R <: Resource: ResourceCompanion](resource: R, owner: Option[String] = None): ZIO[Has[Authentication.Service], CatchEmAllException, R] = {
    for {
      subject <- Authentication.subject
    } yield implicitly[ResourceCompanion[R]].updateMetadata(resource) { metadata =>
      metadata.copy(
        owner = owner.getOrElse(subject),
        createUser = subject,
        updateUser = subject
      )
    }
  }

  def updateAuthenticated[R <: Resource: ResourceCompanion](resource: R): ZIO[Has[Authentication.Service], CatchEmAllException, R] = {
    for {
      subject <- Authentication.subject
    } yield implicitly[ResourceCompanion[R]].updateMetadata(resource) { metadata =>
      metadata.copy(
        updateUser = subject
      )
    }
  }
}

object ResourceUtil extends ResourceUtil
