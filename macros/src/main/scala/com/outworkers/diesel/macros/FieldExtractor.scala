package com.outworkers.diesel.macros

trait FieldExtractor[Table, Col] {
  def fields: Seq[Col]
}

object FieldExtractor {
  implicit def extractor[T] = macro FieldExtractorMacro.fieldsImpl[T]
}

@macrocompat.bundle
class FieldExtractorMacro(val c: scala.reflect.macros.blackbox.Context) {
  import c.universe._

  def fieldsImpl[Table : c.WeakTypeTag, Col : c.TypeTag]: Tree = {
    val tpe = weakTypeOf[Table]
    val colTpe = weakTypeOf[Col]
    val members: Seq[c.Symbol] = (for {
      baseClass <- tpe.baseClasses
      symbol <- baseClass.typeSignature.members.sorted
      if symbol.typeSignature <:< c.typeOf[Col]
    } yield symbol)(collection.breakOut)

    val objects = members.distinct.map(_.name.toTypeName)

    q"""new com.outworkers.diesel.macros.FieldExtractor[$tpe, $colTpe] {
       def fields: Seq[$colTpe] = {Seq(..$objects)}
    }"""
  }
}

