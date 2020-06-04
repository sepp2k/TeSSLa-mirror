package de.uni_luebeck.isp.tessla

import cats.data.Ior
import de.uni_luebeck.isp.tessla.TesslaAST.Core
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}

import scala.collection.mutable

object FlattenCore extends TranslationPhase[Core.Specification, Core.Specification] {

  override def translate(spec: Core.Specification): Result[Core.Specification] = {

    object IdentifierFactory {
      private var _id: Long = spec.maxIdentifier

      def next: Core.Identifier = {
        _id += 1
        new Core.Identifier(Ior.right(_id))
      }

      def count: Long = _id
    }

    def flattenArg(e: Core.ExpressionArg, defs: mutable.Map[Core.Identifier, Core.Expression]): Core.ExpressionArg =
      e match {
        case r: Core.ExpressionRef => r
        case lit @ (_: Core.IntLiteralExpression | _: Core.StringLiteralExpression | _: Core.FloatLiteralExpression |
            Core.ExternExpression("true", _, _) | Core.ExternExpression("false", _, _)) =>
          lit
        case rec @ Core.RecordConstructorExpression(m, _) if m.isEmpty =>
          flattenExpression(rec, defs)
        case typeApp: Core.TypeApplicationExpression => flattenExpression(typeApp, defs)
        case e: Core.Expression =>
          val newID = IdentifierFactory.next
          val flattenedExp = flattenExpression(e, defs)
          defs.addOne(newID -> flattenedExp)
          Core.ExpressionRef(newID, e.tpe)
      }

    def flattenExpression(e: Core.Expression, defs: mutable.Map[Core.Identifier, Core.Expression]): Core.Expression =
      e match {
        case fun: Core.FunctionExpression =>
          val funDefs: mutable.Map[Core.Identifier, Core.Expression] = mutable.Map()
          val result = flattenArg(fun.result, funDefs)
          val body = flattenDefinitions(fun.body ++ funDefs)
          fun.copy(body = body, result = result)
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

    def flattenDefinitions(defs: Map[Core.Identifier, Core.Expression]): Map[Core.Identifier, Core.Expression] = {
      val addDefs = mutable.Map[Core.Identifier, Core.Expression]()
      defs.view.mapValues(flattenExpression(_, addDefs)).toMap ++ addDefs.toMap
    }

    Success(
      Core.Specification(spec.in, flattenDefinitions(spec.definitions), spec.out, IdentifierFactory.count),
      Seq()
    )
  }

}
