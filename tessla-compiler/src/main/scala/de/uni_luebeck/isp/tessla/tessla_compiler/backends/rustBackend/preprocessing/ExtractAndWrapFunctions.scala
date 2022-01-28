package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.preprocessing

import cats.data.Ior
import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TesslaAST.StrictEvaluation
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.Success

import scala.collection.immutable.{ArraySeq, Map}
import scala.collection.mutable

class ExtractAndWrapFunctions extends TranslationPhase[Specification, Specification] {

  private var nextId: Long = _
  private def nextIdentifier(name: Option[String] = None): Identifier = {
    nextId += 1
    name match {
      case Some(name) => new Identifier(Ior.both(name, nextId))
      case None       => new Identifier(Ior.right(nextId))
    }
  }
  private def appendToIdentifier(name: Identifier, suffix: String): Identifier = Identifier(name.idOrName match {
    case Ior.Left(name)      => Ior.left(name + suffix)
    case Ior.Right(num)      => Ior.both(suffix, num)
    case Ior.Both(name, num) => Ior.both(name + suffix, num)
  })

  private val argumentScopeMap = mutable.Stack.empty[mutable.Map[Identifier, (Identifier, Type)]]
  private val additionalDefinitions = mutable.Map.empty[Identifier, DefinitionExpression]
  private val nameStack = mutable.Stack.empty[Identifier]

  private val recursivelyCalledNames = mutable.Set.empty[Identifier]

  private var spec: Specification = _
  private def inGlobalScope(name: Identifier): Boolean = {
    spec.definitions.contains(name) || spec.in.contains(name) || additionalDefinitions.contains(name)
  }

  override def translate(spec: Specification): TranslationPhase.Result[Specification] = {
    this.spec = spec
    nextId = spec.maxIdentifier

    argumentScopeMap.clear()
    additionalDefinitions.clear()
    nameStack.clear()
    recursivelyCalledNames.clear()

    val definitions = spec.definitions.map {
      case (id, definition) =>
        nameStack.push(id)
        val expression = extractFunctionExpressions(definition, _ => false, isGlobalDefinition = true)
        if (nameStack.pop() != id) {
          System.err.println("Uneven name defs")
        }
        id -> expression.asInstanceOf[DefinitionExpression]
    }

    Success(
      Specification(spec.annotations, spec.in, definitions ++ additionalDefinitions, spec.out, nextId),
      Seq()
    )
  }

  /**
   * Recursively traverse through a DefinitionExpression and replace all [[FunctionExpression]]s with
   * a boxed lambda calling an [[ExpressionRef]] pointing to a global function. // TODO box those
   * If the function in question requires data from the surrounding scope, we add those as parameters to the global
   * function and box that data within a clojure.
   * The extracted global functions have all references to variables outside its scope redirected to additional arguments,
   * which are partially applied in the boxed closure.
   *
   * fn foo(a, b) {
   *   bar = (x) {
   *     x + a
   *   }
   *   bar(b)
   * }
   *
   * This is supposed to be transformed into -->
   *
   * fn bar_(p) {
   *   Box::new(move (x) {
   *     x + p
   *   })
   * }
   *
   * fn foo(a, b) {
   *   bar = bar_(a)
   *   bar(b)
   * }
   *
   * @param id
   * @param e
   * @param isGlobalDefinition
   * @return
   */
  def extractFunctionExpressions(
    e: ExpressionArg,
    inCurrentScope: Identifier => Boolean,
    isGlobalDefinition: Boolean = false
  ): ExpressionArg = {
    e match {
      case ExpressionRef(id, tpe: FunctionType, location) if nameStack.contains(id) =>
        recursivelyCalledNames.addOne(id)
        ExpressionRef(id, tpe, location)

      case function: FunctionExpression =>
        // FIXME: here we assume that all functions have a name before being called, is that correct?
        boxScopedDependencies(function, isGlobalDefinition)

      case ApplicationExpression(applicable, args, location) =>
        // TODO these should try finding out what is being called
        ApplicationExpression(
          extractFunctionExpressions(applicable, inCurrentScope),
          args.map { exprArg =>
            extractFunctionExpressions(exprArg, inCurrentScope)
          },
          location
        )
      case TypeApplicationExpression(applicable, typeArgs, location) =>
        TypeApplicationExpression(
          extractFunctionExpressions(applicable, inCurrentScope, isGlobalDefinition),
          typeArgs,
          location
        )
      case RecordAccessorExpression(name, target, nameLocation, location) =>
        RecordAccessorExpression(
          name,
          extractFunctionExpressions(target, inCurrentScope),
          nameLocation,
          location
        )
      case RecordConstructorExpression(entries, location) =>
        RecordConstructorExpression(
          entries.map {
            case (name, (exprArg, location)) =>
              name -> (extractFunctionExpressions(exprArg, inCurrentScope), location)
          },
          location
        )

      case ExpressionRef(id, tpe, location) =>
        // TODO handle reference to function somewhere in outer scope
        if (inGlobalScope(id) || inCurrentScope(id)) {
          e
        } else if (argumentScopeMap.head.contains(id)) {
          ExpressionRef(argumentScopeMap.head(id)._1, tpe, location)
        } else {
          val cleanName = nextIdentifier()
          argumentScopeMap.head.addOne(id -> (cleanName, tpe))
          ExpressionRef(cleanName, tpe, location)
        }

      case extern: ExternExpression        => extern
      case float: FloatLiteralExpression   => float
      case int: IntLiteralExpression       => int
      case string: StringLiteralExpression => string
    }
  }

  private def boxScopedDependencies(
    function: FunctionExpression,
    isGlobalDefinition: Boolean = false
  ): ExpressionArg = function match {
    case FunctionExpression(typeParams, params, body, result, location) =>
      argumentScopeMap.push(mutable.Map.empty[Identifier, (Identifier, Type)])

      def inCurrentScope(name: Identifier): Boolean = {
        params.exists { case (id, _, _) => id == name } || body.exists { case (id, _) => id == name }
      }

      val sanitizedBody = body.map {
        case (name, definition) =>
          nameStack.push(name)
          val expression = extractFunctionExpressions(definition, inCurrentScope)
          if (nameStack.pop() != name) {
            System.err.println("Uneven name defs")
          }
          name -> expression.asInstanceOf[DefinitionExpression]
      }
      val sanitizedResult =
        extractFunctionExpressions(result, inCurrentScope)

      val remappedScope = argumentScopeMap.pop()

      // TODO:
      //  fun name_extracted(arg1, arg2, scope1, scope2) { ... }
      //  fun name_boxing(scope1, scope2) { Box::new(move |arg1, arg2| name_extracted(arg1, arg2, scope1, scope2)) }
      //

      if (isGlobalDefinition) {
        // this function is already defined globally
        FunctionExpression(typeParams, params, sanitizedBody, sanitizedResult, location)
      } else {
        // this function is not defined globally

        val remappedParams = remappedScope.toList.map { case (_, (param, typ)) => (param, StrictEvaluation, typ) }

        val name = nameStack.head

        val functionName = appendToIdentifier(name, "$fun")
        val functionExpr = FunctionExpression(
          typeParams,
          params ++ remappedParams,
          sanitizedBody,
          sanitizedResult,
          location
        )

        if (recursivelyCalledNames.contains(name)) {
          additionalDefinitions.addOne(
            functionName -> resolveRecursiveCall(
              name,
              functionName,
              functionExpr.tpe,
              remappedScope.toSeq.map { case (_, (param, typ)) => ExpressionRef(param, typ) },
              functionExpr
            ).asInstanceOf[DefinitionExpression]
          )
        } else {
          additionalDefinitions.addOne(functionName -> functionExpr)
        }

        val boxName = appendToIdentifier(name, "$box")
        val boxExpr = FunctionExpression(
          typeParams,
          remappedParams,
          Map(),
          boxExpression(
            FunctionExpression(
              typeParams,
              params,
              Map(),
              ApplicationExpression(
                TypeApplicationExpression(
                  ExpressionRef(functionName, functionExpr.tpe, location),
                  typeParams.map { typeName => TypeParam(typeName) }
                ),
                ArraySeq.from(
                  params.map { case (id, _, typ) => ExpressionRef(id, typ) }
                    ++ remappedScope.map { case (_, (param, typ)) => ExpressionRef(param, typ) }
                ),
                location
              ),
              location
            )
          )
        )
        additionalDefinitions.addOne(boxName -> boxExpr)

        ApplicationExpression(
          TypeApplicationExpression(
            ExpressionRef(boxName, boxExpr.tpe, location),
            typeParams.map { typeName => TypeParam(typeName) }
          ),
          ArraySeq.from(remappedScope.map {
            case (id, (_, typ)) => ExpressionRef(id, typ, location)
          }),
          location
        )
      }
  }

  private def boxExpression(expr: ExpressionArg): ApplicationExpression = {
    ApplicationExpression(
      TypeApplicationExpression(
        ExternExpression(
          "[rust]box",
          FunctionType(
            List(),
            List((StrictEvaluation, expr.tpe)),
            expr.tpe
          )
        ),
        List()
      ),
      ArraySeq(expr)
    )
  }

  private def resolveRecursiveCall(
    initialName: Identifier,
    resolvedName: Identifier,
    resolvedType: Type,
    additionalArgs: Seq[ExpressionRef],
    expression: ExpressionArg
  ): ExpressionArg = expression match {
    case ApplicationExpression(
          TypeApplicationExpression(ExpressionRef(id, _: FunctionType, _), typeArgs, _),
          args,
          location
        ) if id == initialName =>
      ApplicationExpression(
        TypeApplicationExpression(ExpressionRef(resolvedName, resolvedType), typeArgs),
        args ++ additionalArgs,
        location
      )

    case FunctionExpression(typeParams, params, body, result, location) =>
      FunctionExpression(
        typeParams,
        params,
        body.map {
          case (id, expr) =>
            id -> resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, expr)
              .asInstanceOf[DefinitionExpression]
        },
        resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, result),
        location
      )

    case ApplicationExpression(applicable, args, location) =>
      ApplicationExpression(
        resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, applicable),
        args.map { exprArg =>
          resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, exprArg)
        },
        location
      )
    case TypeApplicationExpression(applicable, typeArgs, location) =>
      TypeApplicationExpression(
        resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, applicable),
        typeArgs,
        location
      )
    case RecordAccessorExpression(name, target, nameLocation, location) =>
      RecordAccessorExpression(
        name,
        resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, target),
        nameLocation,
        location
      )
    case RecordConstructorExpression(entries, location) =>
      RecordConstructorExpression(
        entries.map {
          case (name, (exprArg, location)) =>
            name -> (resolveRecursiveCall(initialName, resolvedName, resolvedType, additionalArgs, exprArg), location)
        },
        location
      )

    case ref: ExpressionRef              => ref
    case extern: ExternExpression        => extern
    case float: FloatLiteralExpression   => float
    case int: IntLiteralExpression       => int
    case string: StringLiteralExpression => string
  }
}
