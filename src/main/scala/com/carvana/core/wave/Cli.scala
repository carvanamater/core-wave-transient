package com.carvana.core.wave

import zio.ZManaged

trait Cli {

  case class Commands[Env](lookup: Map[String, ZManaged[Env, Throwable, Any]] = Map[String, ZManaged[Env, Throwable, Any]]()) {
    def withCommand(name: String)(zm: ZManaged[Env, Throwable, Any]): Commands[Env] = copy(lookup = lookup + (name -> zm))

    def build(commands: Seq[String]): ZManaged[Env, Throwable, Any] = ZManaged.collectAll(lookup.filter(t => commands.contains(t._1)).values)
  }
}

object Cli extends Cli
