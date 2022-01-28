package com.carvana.core.wave.jresource

import magnolia1._
import zio.json.{jsonExclude, jsonField}

import scala.language.experimental.macros

object DeriveSchema {

  implicit def gen[A]: Schema[A] = macro Magnolia.gen[A]

  type Typeclass[A] = Schema[A]

  def join[A](ctx: CaseClass[Schema, A]): Schema[A] = {
    val params = ctx.parameters
      .filterNot(p => p.annotations.exists(_.isInstanceOf[jsonExclude]))

    var schema = Schema.Product[A](
      name = Option(typeNameToSchemaName(ctx.typeName, ctx.annotations))
    )

    schema = ctx.annotations.collectFirst { case a: jsDescription => a }.fold(schema)(v => schema.description(v.description))
    params.foldLeft(schema) { (acc, param) =>
      val name = param.annotations.collectFirst { case a: jsonField => a.name }.getOrElse(param.label)
      val annotations = param.annotations.collect { case a: SchemaAnnotation => a }
      val schema = annotations.foldLeft(param.typeclass)(_.annotate(_))
      acc.field(name, Schema.Product.Field[A, param.PType](schema, c => Option(param.dereference(c))))
    }
  }

  private def typeNameToSchemaName(typeName: magnolia1.TypeName, annotations: Seq[Any]): String = {
    annotations.collectFirst { case ann: jsName => ann.name } match {
      case Some(altName) => altName
      case None          => typeName.short
    }
  }

}
