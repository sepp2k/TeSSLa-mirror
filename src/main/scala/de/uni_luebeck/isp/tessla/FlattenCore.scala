package de.uni_luebeck.isp.tessla

import cats.data.Ior
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

import scala.collection.mutable

object FlattenCore extends TranslationPhase[Core.Specification, Core.Specification] {

  type Definitions = mutable.Map[Core.Identifier, Core.Expression]

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    object IdentifierFactory {
      private var _id: Long = spec.maxIdentifier

      def next: Core.Identifier = {
        _id += 1
        new Core.Identifier(Ior.right(_id))
      }

      def count: Long = _id
    }

    def addDefinition(e: Core.Expression, defs: Definitions): Core.ExpressionRef = {
      val newID = IdentifierFactory.next
      val flattenedExp = flattenExpression(e, defs)
      defs += newID -> flattenedExp
      Core.ExpressionRef(newID, e.tpe)
    }

    def flattenArg(e: Core.ExpressionArg, defs: Definitions): Core.ExpressionArg =
      e match {
        case r: Core.ExpressionRef => r
        case lit @ (_: Core.IntLiteralExpression | _: Core.StringLiteralExpression | _: Core.FloatLiteralExpression |
            Core.ExternExpression("true", _, _) | Core.ExternExpression("false", _, _)) =>
          lit
        case rec @ Core.RecordConstructorExpression(m, _) if m.isEmpty =>
          flattenExpression(rec, defs)
        case typeApp: Core.TypeApplicationExpression => flattenExpression(typeApp, defs)
        case ext: Core.ExternExpression =>
          defs
            .collectFirst {
              case (id, ext2: Core.ExternExpression) if ext.name == ext2.name => Core.ExpressionRef(id, ext.tpe)
            }
            .getOrElse(addDefinition(ext, defs))
        case e: Core.Expression => addDefinition(e, defs)
      }

    def flattenExpression(e: Core.Expression, defs: Definitions): Core.Expression =
      e match {
        case fun: Core.FunctionExpression =>
          val funDefs: mutable.Map[Core.Identifier, Core.Expression] = mutable.Map()
          val result = flattenArg(fun.result, funDefs)
          val body = flattenDefinitions(fun.body ++ funDefs)
          fun.copy(body = body.toMap, result = result)
        case app: Core.ApplicationExpression =>
          app.copy(flattenArg(app.applicable, defs), app.args.map(flattenArg(_, defs)))
        case typeApp: Core.TypeApplicationExpression =>
          typeApp.copy(flattenArg(typeApp.applicable, defs))
        case rec: Core.RecordConstructorExpression =>
          rec.copy(rec.entries.map { case (n, (e, loc)) => (n, (flattenArg(e, defs), loc)) })
        case acc: Core.RecordAccessorExpression =>
          acc.copy(target = flattenArg(acc.target, defs))
        case _ => e
      }

    def flattenDefinitions(defs: Map[Core.Identifier, Core.Expression]): Definitions = {
      val addDefs = mutable.Map[Core.Identifier, Core.Expression]()
      val flattened = defs.view.mapValues(flattenExpression(_, addDefs)).toMap
      addDefs ++ flattened
    }

    Success(
      Core.Specification(spec.in, flattenDefinitions(spec.definitions).toMap, spec.out, IdentifierFactory.count),
      Seq()
    )
  }

}
