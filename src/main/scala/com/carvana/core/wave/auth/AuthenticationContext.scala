package com.carvana.core.wave.auth

sealed trait AuthenticationContext {
  def isAuthenticated: Boolean

  def maybeSubject: Option[String]
}

object AuthenticationContext {

  sealed trait WithSubject {
    def subject: String
  }

  case object Anonymous extends AuthenticationContext {
    override def isAuthenticated: Boolean = false

    override def maybeSubject: Option[String] = None
  }

  case class Unauthenticated(override val subject: String) extends AuthenticationContext with WithSubject {
    override def isAuthenticated: Boolean = false

    override def maybeSubject: Option[String] = Option(subject)
  }

  case class Authenticated(override val subject: String) extends AuthenticationContext with WithSubject {
    override def isAuthenticated: Boolean = true

    override def maybeSubject: Option[String] = Option(subject)
  }

  def apply(isAuthenticated: Boolean, subject: Option[String]): AuthenticationContext = if (isAuthenticated) {
    subject.map(Authenticated).getOrElse(Anonymous)
  } else {
    subject.map(Unauthenticated).getOrElse(Anonymous)
  }
}
