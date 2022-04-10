package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.preprocessing

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core._
import de.uni_luebeck.isp.tessla.core.TranslationPhase
import de.uni_luebeck.isp.tessla.core.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend.RustUtils
import de.uni_luebeck.isp.tessla.tessla_compiler.{
  DefinitionOrdering,
  Diagnostics,
  ExtendedSpecification,
  TypeArgManagement
}

/**
 * This preprocessing step analyses uses of generic types in extern functions to determine which Rust traits they must
 * possess to allow for using the type with the associated Rust functions or operators
 */
object InferGenericTypeTraits extends TranslationPhase[ExtendedSpecification, ExtendedSpecification] {
  override def translate(extSpec: ExtendedSpecification): Result[ExtendedSpecification] = {
    val traitInfo = DefinitionOrdering
      .order(extSpec.spec.definitions)
      .foldLeft(Map.empty[Identifier, Set[String]]) {
        case (traitInfo, (_, expr)) =>
          mergeMap(traitInfo, analyzeGenericTypes(expr, traitInfo, TypeArgManagement.empty))
      }

    Success(
      ExtendedSpecification(extSpec.spec, extSpec.usageInfo, extSpec.lazyVars, extSpec.inlining, Some(traitInfo)),
      Seq()
    )
  }

  /**
   * Source: https://stackoverflow.com/a/59350577/4704639
   */
  private def mergeMap[A, B](map1: Map[A, Set[B]], map2: Map[A, Set[B]]): Map[A, Set[B]] =
    (map1.keySet ++ map2.keySet).map(x => (x, map1.getOrElse(x, Set.empty) ++ map2.getOrElse(x, Set.empty))).toMap
  private def mergeMaps[A, B](maps: Iterable[Map[A, Set[B]]]): Map[A, Set[B]] =
    if (maps.isEmpty) Map.empty else maps.reduce((a, b) => mergeMap(a, b))

  /**
   * Recursively analyse the use of externs on generic types, and accumulate a set of required traits for these generics
   *
   * @param expr The expression to traverse
   * @param traitInfo A Map of trait bounds we already know
   * @param tm A helper object handling generic type application
   * @return A map of required trait bounds found in the current expression
   */
  private def analyzeGenericTypes(
    expr: ExpressionArg,
    traitInfo: Map[Identifier, Set[String]],
    tm: TypeArgManagement
  ): Map[Identifier, Set[String]] =
    expr match {
      case ExternExpression(name, typ: FunctionType, _) =>
        getRequiredTraitsFromExtern(s"__${name}__", typ, tm.parsKnown(typ.typeParams))

      case ApplicationExpression(TypeApplicationExpression(applicable, typeArgs, _), args, _) =>
        val innerInfo = mergeMaps(
          analyzeGenericTypes(applicable, traitInfo, tm.typeApp(typeArgs))
            +: args.map(analyzeGenericTypes(_, traitInfo, tm))
        )
        mergeMaps(args.zip(applicable.tpe.asInstanceOf[FunctionType].paramTypes).map {
          case (argExpr, (_, paramType)) =>
            // compare each application argument's type with the matching function parameter type to figure out which generic types match
            applyTraitsFromFunctionCall(argExpr.tpe, paramType, mergeMap(innerInfo, traitInfo))
        } :+ innerInfo)

      case FunctionExpression(typeParams, _, body, result, _) =>
        val innerTm = tm.parsKnown(typeParams)
        mergeMaps(DefinitionOrdering.order(body).map {
          case (_, expr) => analyzeGenericTypes(expr, traitInfo, innerTm)
        } :+ analyzeGenericTypes(result, traitInfo, innerTm))

      case TypeApplicationExpression(applicable, typeArgs, _) =>
        analyzeGenericTypes(applicable, traitInfo, tm.typeApp(typeArgs))
      case ApplicationExpression(applicable, args, _) =>
        mergeMaps(analyzeGenericTypes(applicable, traitInfo, tm) +: args.map(analyzeGenericTypes(_, traitInfo, tm)))
      case RecordAccessorExpression(_, target, _, _) =>
        analyzeGenericTypes(target, traitInfo, tm)
      case RecordConstructorExpression(entries, _) =>
        mergeMaps(entries.map { case (_, (expr, _)) => analyzeGenericTypes(expr, traitInfo, tm) })
      case _: ExpressionRef | _: ExternExpression | _: StringLiteralExpression | _: IntLiteralExpression |
          _: FloatLiteralExpression =>
        Map.empty
    }

  /**
   * A function that associates needed traits to any generics used in known extern functions
   * @param name The extern's name
   * @param typ The extern type at the call-site
   * @param tm A helper object handling generic type application
   * @return A Map of generic types to a Set of their trait bounds
   */
  private def getRequiredTraitsFromExtern(
    name: String,
    typ: FunctionType,
    tm: TypeArgManagement
  ): Map[Identifier, Set[String]] = {
    val paramTypes = typ.paramTypes.map { case (_, typ) => typ.resolve(tm.resMap) }.toIndexedSeq
    val resultType = typ.resultType.resolve(tm.resMap)

    def typeNeedsTraits(typ: Type, traits: Set[String]): Map[Identifier, Set[String]] =
      RustUtils.getGenericTypeNames(Seq(typ)).map { generic => generic -> traits }.toMap

    def argNeedsTraits(index: Int, traits: Set[String]): Map[Identifier, Set[String]] =
      typeNeedsTraits(paramTypes(index), traits)

    def invalidTypeError(typ: Type): Diagnostics.CommandNotSupportedError =
      Diagnostics.CommandNotSupportedError(s"Encountered unknown or invalid type: $typ", typ.location)

    name match {
      // TODO it might be nice to give a warning here that generics X,Y may need additional traits
      case s if s.startsWith("__native:") => Map.empty

      // stream functions
      case "__nil__" | "__default__" | "__defaultFrom__" | "__time__" | "__last__" | "__delay__" | "__lift__" |
          "__slift__" | "__merge__" | "__count__" | "__const__" | "__filter__" | "__fold__" | "__reduce__" |
          "__unitIf__" | "__pure__" =>
        Map.empty

      // rust specific helper
      case "__[rust]format__" => argNeedsTraits(1, Set("TesslaDisplay"))

      // operators
      case "__ite__" | "__staticite__"  => Map.empty
      case "__and__"                    => Map.empty
      case "__or__"                     => Map.empty
      case "__not__" | "__bitflip__"    => Map.empty
      case "__negate__" | "__fnegate__" => Map.empty
      case "__eq__"                     => argNeedsTraits(0, Set("PartialEq"))
      case "__neq__"                    => argNeedsTraits(0, Set("PartialEq"))
      case "__gt__" | "__fgt__" =>
        mergeMaps(Seq(argNeedsTraits(0, Set("PartialOrd")), argNeedsTraits(1, Set("PartialOrd"))))
      case "__lt__" | "__flt__" =>
        mergeMaps(Seq(argNeedsTraits(0, Set("PartialOrd")), argNeedsTraits(1, Set("PartialOrd"))))
      case "__geq__" | "__fgeq__" =>
        mergeMaps(Seq(argNeedsTraits(0, Set("PartialOrd")), argNeedsTraits(1, Set("PartialOrd"))))
      case "__leq__" | "__fleq__" =>
        mergeMaps(Seq(argNeedsTraits(0, Set("PartialOrd")), argNeedsTraits(1, Set("PartialOrd"))))
      case "__add__" | "__fadd__" => Map.empty
      case "__String_concat__"    => Map.empty
      case "__sub__" | "__fsub__" => Map.empty
      case "__mul__" | "__fmul__" => Map.empty
      case "__div__" | "__fdiv__" => Map.empty
      case "__mod__"              => Map.empty
      case "__bitand__"           => Map.empty
      case "__bitor__"            => Map.empty
      case "__bitxor__"           => Map.empty
      case "__leftshift__"        => Map.empty
      case "__rightshift__"       => Map.empty
      case "__abs__" | "__fabs__" => Map.empty

      // float functions
      case "__pow__" | "__log__" | "__sin__" | "__cos__" | "__tan__" | "__atan__" => Map.empty

      case "__intToFloat__" | "__floatToInt__" => Map.empty

      // option functions
      case "__Some__" | "__None__" | "__getSome__" | "__isSome__" | "__isNone__" => Map.empty

      // string functions
      case "__toString__"       => argNeedsTraits(0, Set("TesslaDisplay"))
      case "__String_toUpper__" => Map.empty
      case "__String_toLower__" => Map.empty

      // map functions
      case "__Map_empty__" =>
        resultType match {
          case InstantiatedType("Map", List(keyType, _), _) => typeNeedsTraits(keyType, Set("Eq", "Hash"))
          case other                                        => throw invalidTypeError(other)
        }
      case "__Map_add__" | "__Map_contains__" | "__Map_get__" | "__Map_remove__" | "__Map_size__" | "__Map_keys__" |
          "__Map_fold__" =>
        paramTypes(0) match {
          case InstantiatedType("Map", List(keyType, _), _) => typeNeedsTraits(keyType, Set("Eq", "Hash"))
          case other                                        => throw invalidTypeError(other)
        }
      case "__Map_map__" =>
        mergeMaps(
          Seq(
            paramTypes(0) match {
              case InstantiatedType("Map", List(keyType, _), _) => typeNeedsTraits(keyType, Set("Eq", "Hash"))
              case other                                        => throw invalidTypeError(other)
            },
            resultType match {
              case InstantiatedType("Map", List(keyType, _), _) => typeNeedsTraits(keyType, Set("Eq", "Hash"))
              case other                                        => throw invalidTypeError(other)
            }
          )
        )

      // set functions
      case "__Set_empty__" =>
        resultType match {
          case InstantiatedType("Set", List(entryType), _) => typeNeedsTraits(entryType, Set("Eq", "Hash"))
          case other                                       => throw invalidTypeError(other)
        }
      case "__Set_add__" | "__Set_contains__" | "__Set_remove__" | "__Set_size__" | "__Set_union__" |
          "__Set_intersection__" | "__Set_minus__" | "__Set_fold__" =>
        paramTypes(0) match {
          case InstantiatedType("Set", List(entryType), _) => typeNeedsTraits(entryType, Set("Eq", "Hash"))
          case other                                       => throw invalidTypeError(other)
        }
      case "__Set_map__" =>
        mergeMaps(
          Seq(
            resultType match {
              case InstantiatedType("Set", List(entryType), _) => typeNeedsTraits(entryType, Set("Eq", "Hash"))
              case other                                       => throw invalidTypeError(other)
            },
            paramTypes(0) match {
              case InstantiatedType("Set", List(entryType), _) => typeNeedsTraits(entryType, Set("Eq", "Hash"))
              case other                                       => throw invalidTypeError(other)
            }
          )
        )

      // list functions
      case "__List_empty__" | "__List_size__" | "__List_append__" | "__List_prepend__" | "__List_tail__" |
          "__List_init__" | "__List_get__" | "__List_map__" | "__List_set__" | "__List_fold__" =>
        Map.empty

      case "__error__" => Map.empty

      case _ =>
        throw Diagnostics.CommandNotSupportedError(
          s"Encountered unknown built-in function trying to infer Rust traits: $name"
        )
    }
  }

  /**
   * Compare the type of an applied argument to the function's parameter type to copy required traits from matching generic types
   * @param argType the applied argument type
   * @param paramType the type of the function parameter
   * @param traitInfo The Map of known trait bounds
   * @return A Map of any additional trait bounds found
   */
  private def applyTraitsFromFunctionCall(
    argType: Type,
    paramType: Type,
    traitInfo: Map[Identifier, Set[String]]
  ): Map[Identifier, Set[String]] = {
    paramType match {
      case TypeParam(name, _) =>
        traitInfo.get(name) match {
          case Some(traits) => RustUtils.getGenericTypeNames(Seq(argType)).map { generic => generic -> traits }.toMap
          case None         => Map.empty
        }
      case RecordType(paramEntries, _) =>
        argType match {
          case RecordType(argEntries, _) if paramEntries == argEntries =>
            mergeMaps(argEntries.zip(paramEntries).map {
              case ((_, (argType, _)), (_, (paramType, _))) =>
                applyTraitsFromFunctionCall(argType, paramType, traitInfo)
            })
          case _ =>
            throw Diagnostics.CommandNotSupportedError(
              s"Failed to match arg type ($argType) to param type ($paramType)",
              argType.location
            )
        }
      case FunctionType(_, paramTypes, resultType, _) =>
        argType match {
          case FunctionType(_, argParamTypes, argResultType, _) =>
            mergeMaps(argParamTypes.zip(paramTypes).map {
              case ((_, argType), (_, paramType)) =>
                applyTraitsFromFunctionCall(argType, paramType, traitInfo)
            } ++ List(applyTraitsFromFunctionCall(argResultType, resultType, traitInfo)))
          case _ =>
            throw Diagnostics.CommandNotSupportedError(
              s"Failed to match arg type ($argType) to param type ($paramType)",
              argType.location
            )
        }
      case InstantiatedType(paramName, typeArgs, _) =>
        argType match {
          case InstantiatedType(argName, argTypeArgs, _) if paramName == argName =>
            mergeMaps(argTypeArgs.zip(typeArgs).map {
              case (argType, paramType) => applyTraitsFromFunctionCall(argType, paramType, traitInfo)
            })
          case _ =>
            throw Diagnostics.CommandNotSupportedError(
              s"Failed to match arg type ($argType) to param type ($paramType)",
              argType.location
            )
        }
    }
  }
}
