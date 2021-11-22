package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

import de.uni_luebeck.isp.tessla.core.TesslaAST.Core.{FunctionType => _, _}
import de.uni_luebeck.isp.tessla.core.TesslaAST.{Core, LazyEvaluation, StrictEvaluation}
import de.uni_luebeck.isp.tessla.tessla_compiler.{Diagnostics, ExtendedSpecification, IntermediateCodeUtils}
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCodeUtils.{
  DeclarationType,
  FinalDeclaration,
  FinalLazyDeclaration,
  VariableDeclaration
}

class RustCodeGenerator(extSpec: ExtendedSpecification) {

  /**
   * Set of identifiers which are assigned with final assignments.
   * If an assignment uses exclusively existing assignments it can also be finally assigned
   * Otherwise (in case of recursion) it must be assigned with a dummy default value first
   */
  private var definedIdentifiers: Map[Identifier, DeclarationType] = Map()

  /**
   * Translates a function application to ImpLan
   * @param e The function expression which is applied
   * @param args The argument expressions of the application
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function application
   */
  def translateFunctionCall(
    e: ExpressionArg,
    args: Seq[String],
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): String = {
    ""
  }

  /**
   * Translates an assignment from TeSSLa to Rust
   * @param id The id which is assigned
   * @param e The expression assigned to id
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @return The translated assignment
   */
  def translateAssignment(
    id: Identifier,
    e: ExpressionArg,
    tm: TypeArgManagement
  ): String = {
    val resTpe = e.tpe.resolve(tm.resMap)
    val lazyVar = extSpec.lazyVars.get.contains(id)
    if (lazyVar || finalAssignmentPossible(e)) {
      definedIdentifiers += (id -> (if (lazyVar) FinalLazyDeclaration else FinalDeclaration))
      val inlinedExp = e //if (lazyVar) inlineVars(e, extSpec.spec.definitions) else e
      s"const var_$id = ${translateExpressionArg(inlinedExp, tm, extSpec.spec.definitions)};"
    } else {
      definedIdentifiers += (id -> VariableDeclaration)
      s"let var_$id = ${translateExpressionArg(e, tm, extSpec.spec.definitions)};"
    }
  }

  /**
   * Translates a TeSSLa Core FunctionExpression to a Rust expression
   * @param e The function to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated function expression
   */
  def translateFunction(
    e: FunctionExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): String = {
    ""
  }

  /**
   * Translates an ExpressionArg to a corresponding Rust expression
   * @param e The expression to be translated
   * @param tm The [[TypeArgManagement]] to resolve type parameters
   * @return The translated expression
   */
  def translateExpressionArg(
    e: ExpressionArg,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression] = Map()
  ): String = {
    e match {
      case TypeApplicationExpression(e, tArgs, _) =>
        translateExpressionArg(e, tm.typeApp(tArgs), defContext)
      case f: FunctionExpression =>
        translateFunction(f, tm, defContext)
      /*case ApplicationExpression(e, args, _) =>
        translateFunctionCall(
          e,
          getInlinedArgs(args, defContext).map(translateExpressionArg(_, tm, defContext)),
          tm,
          defContext
        )*/
      case StringLiteralExpression(value, _) =>
        s""""$value""""
      case IntLiteralExpression(value, _) =>
        s"${value.toLong}_i64"
      case FloatLiteralExpression(value, _) =>
        s"${value}_f64"
      case ExpressionRef(id, _, _) =>
        s"var_${id.fullName}"
      case x: ExternExpression =>
        translateExtern(x, tm, defContext)
      case RecordConstructorExpression(entries, _) if entries.isEmpty =>
        "()" // Unit value
      /*case RecordConstructorExpression(entries, _) =>
        MkStruct(entries.toSeq.map { case (n, (ea, _)) => (n.name, translateExpressionArg(ea, tm, defContext)) }, e.tpe)
      case RecordAccessorExpression(name, target, _, _) =>
        GetStruct(translateExpressionArg(target, tm, defContext), name.name, target.tpe)*/
      case _ =>
        throw Diagnostics.CoreASTError("Unexpected ExpressionArg cannot be translated", e.location)
    }
  }

  /**
   * Translates an ExternExpression. If the extern is of function type a lambda expression is wrapped around it.
   * If the extern is directly applied this lambda is most likely unnecessary and this function should not be
   * used for translation of the called extern.
   * @param e          The[[ExternExpression]]
   * @param tm         The [[TypeArgManagement]] to resolve type parameters
   * @param defContext Definition context depicting all var names in the current scope to their definition expression
   * @return The translated expression
   */
  def translateExtern(
    e: ExternExpression,
    tm: TypeArgManagement,
    defContext: Map[Identifier, DefinitionExpression]
  ): String = {
    e match {
      case ExternExpression("true", _, _) =>
        "true"
      case ExternExpression("false", _, _) =>
        "false"
      case ExternExpression("None", InstantiatedType("Option", Seq(t), _), _) =>
        "None"
      /*case ExternExpression(_, typ: Core.FunctionType, _) =>
        val newTm = tm.parsKnown(typ.typeParams)
        val argNames = typ.paramTypes.indices.map(i => s"tLPar_$i")
        val argTypes = typ.paramTypes
          .map(_._2.resolve(newTm.resMap))
          .map(IntermediateCodeUtils.typeConversion)
          .zip(typ.paramTypes.map(_._1))
          .map {
            case (t, StrictEvaluation) => t
            case (t, LazyEvaluation)   => LazyContainer(t)
          }
        val ret = translateFunctionCall(e, argNames.map(Variable), newTm, defContext)
        LambdaExpression(argNames, argTypes, typ.resultType.resolve(newTm.resMap), Seq(ReturnStatement(ret)))*/
    }
  }

  /**
   * Checks whether a given expression only uses identifiers from [[definedIdentifiers]], i.e. those already defined
   * @param e Expression to be examined
   * @return  Whether all subexpressions are already defined and and the expression can hence be evaluated
   */
  private def finalAssignmentPossible(e: ExpressionArg): Boolean = {
    //Boolean parameter indicates whether variable already has to be evaluateable
    def getSubIDs(e: ExpressionArg, ignore: Set[Identifier] = Set()): Set[(Identifier, Boolean)] = e match {
      case FunctionExpression(_, params, body, result, _) =>
        (body.values.toSeq :+ result)
          .map(getSubIDs(_, ignore ++ params.map(_._1) ++ body.keys))
          .reduce(_ ++ _)
          .map(e => (e._1, false))
      case ApplicationExpression(applicable, args, _) =>
        (args :+ applicable).map(getSubIDs(_, ignore)).reduce(_ ++ _)
      case TypeApplicationExpression(applicable, _, _) => getSubIDs(applicable, ignore)
      case RecordConstructorExpression(entries, _) =>
        entries.map(e => getSubIDs(e._2._1, ignore)).fold(Set()) { case (x, y) => x ++ y }
      case RecordAccessorExpression(_, target, _, _)       => getSubIDs(target, ignore)
      case ExpressionRef(id, _, _) if !ignore.contains(id) => Set((id, true))
      case _                                               => Set()
    }

    getSubIDs(e).forall(d =>
      definedIdentifiers.contains(d._1) && (!d._2 || definedIdentifiers(d._1) != VariableDeclaration)
    )
  }

  /**
   * Class for managing type arguments
   * @param resMap Map representing depiction from type arg names to types
   * @param unappliedArgs Types where type application has already been processed but not the expression
   *                      where the types are applied to
   */
  case class TypeArgManagement(resMap: Map[Identifier, Type], unappliedArgs: Seq[Type]) {

    /**
     * Process a type application. The new types are stored in [[unappliedArgs]] until the
     * expression they are applied on is hit. If [[unappliedArgs]] already contains types they
     * are overwritten.
     * @param types The types which are applied to the subexpression
     * @return An updated [[TypeArgManagement]]
     */
    def typeApp(types: Seq[Type]): TypeArgManagement = {
      TypeArgManagement(resMap, types)
    }

    /**
     * Processes the sub-expression after a type application and adds the type arg -> type relation to the
     * [[resMap]]. [[unappliedArgs]] is cleared. The types are connected to the arguments in the order
     * they appeared in the type application.
     * @param pars The type params of the sub-expression. If number does not match previous [[typeApp]] call
     *             as much type args as possible are connected to types.
     * @return An updated [[TypeArgManagement]]
     */
    def parsKnown(pars: Seq[Identifier]): TypeArgManagement = {
      TypeArgManagement(resMap.removedAll(pars) ++ pars.zip(unappliedArgs).toMap, Seq())
    }
  }

  object TypeArgManagement {

    /**
     * Produces a fresh [[TypeArgManagement]] object
     * @return Empty[[TypeArgManagement]]
     */
    def empty: TypeArgManagement = TypeArgManagement(Map(), Seq())
  }
}
