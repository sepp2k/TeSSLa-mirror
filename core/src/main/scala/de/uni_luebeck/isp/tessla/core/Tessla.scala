/*
 * Copyright 2022 The TeSSLa Community
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package de.uni_luebeck.isp.tessla.core

import de.uni_luebeck.isp.tessla.core.Errors.ParserError

/**
 * Contains data structures for the Tessla AST. This is the first AST generated from the parsed result, thus very
 * close to the parsed structure.
 */
object Tessla {

  /**
   * Describes a Tessla specification.
   *
   * @param statements the statements of the specification
   */
  case class Specification(statements: Seq[Statement]) {
    override def toString =
      s"${statements.mkString("\n")}"
  }

  /**
   * A Tessla identifier.
   * @param name the name of the identifier
   * @param loc the location
   */
  case class Identifier(name: String, loc: Location) extends Location.HasLoc {
    override def toString = name
  }

  sealed abstract class Statement extends Location.HasLoc {
    def loc: Location
  }

  /**
   * A Tessla 'def' statement.
   * @param id the name of the definition
   * @param typeParameters the type parameters declared by the definition
   * @param parameters the parameters declared by the definition
   * @param returnType the return type
   * @param headerLoc the location of the definition header
   * @param body the body of the definition
   * @param loc the location of the definition itself
   * @param isLiftable a flag denoting if the definition is liftable or not
   */
  case class Definition(
    id: Identifier,
    typeParameters: Seq[Identifier],
    parameters: Seq[(Option[TesslaAST.RuntimeEvaluation], Parameter)],
    returnType: Option[Type],
    parens: Boolean,
    headerLoc: Location,
    body: Body,
    loc: Location,
    isLiftable: Boolean
  ) extends Statement {
    override def toString = toString(objectNotation = false)

    def toString(objectNotation: Boolean) = {
      val liftable = if (isLiftable) "liftable " else ""
      val typeParameterList =
        if (typeParameters.isEmpty) ""
        else typeParameters.mkString("[", ", ", "]")
      val parameterList =
        if (parameters.isEmpty) ""
        else parameters.mkString("(", ", ", ")")
      val defString = if (objectNotation) "" else "def "
      val assign = if (objectNotation) "=" else ":="
      s"$liftable$defString$id$typeParameterList$parameterList $assign $body"
    }
  }

  /**
   * A Tessla annotation.
   * @param id the name of the annotation
   * @param arguments the arguments declared with it
   * @param loc the location
   */
  case class Annotation(id: Identifier, arguments: Seq[Argument[Expression]], loc: Location) {
    def name: String = id.name

    override def toString: String = s"@$id(${arguments.mkString(", ")})"
  }

  /**
   * Wraps an annotation as a statement for globally scoped annotations.
   * @param annotation the annotation to use as global annotation
   */
  case class GlobalAnnotation(annotation: Annotation) extends Statement {
    override def loc: Location = annotation.loc
  }

  sealed abstract class Body extends Location.HasLoc {
    def loc: Location
  }

  /**
   * A wrapper for an expression used as a body.
   */
  case class ExpressionBody(exp: Expression) extends Body {
    override def toString = exp.toString

    override def loc = exp.loc
  }

  /**
   * An extern definition.
   * @param id the name of the defined extern
   * @param referenceImplementation the reference implementation, if provided.
   */
  case class Extern(id: Identifier, referenceImplementation: Option[Expression]) extends Body {
    override def toString = s"extern($id)"

    override def loc = id.loc
  }

  /**
   * A Tessla module.
   * @param id the name of the module
   * @param contents the statements the module contains
   * @param loc the location
   */
  case class Module(id: Identifier, contents: Seq[Statement], loc: Location) extends Statement {
    override def toString = contents.mkString(s"module $id {\n", "\n", "\n}")

    def name: String = id.name
  }

  /**
   * An import statement.
   * @param path the path describing the module to import
   * @param loc the location of the statement
   */
  case class Import(path: List[Identifier], loc: Location) extends Statement {
    override def toString = s"import ${path.mkString(".")}"
  }

  /**
   * A definition of an annotation
   * @param id the name of the annotation
   * @param parameters the declaration of its parameters
   * @param global a flag denoting if it is a global or stream annotation
   * @param loc the location
   */
  case class AnnotationDefinition(
    id: Identifier,
    parameters: Seq[Parameter],
    global: Boolean,
    loc: Location
  ) extends Statement

  /**
   * A definition of a type
   * @param id the name of the type
   * @param typeParameters its type parameters
   * @param body the body of the definition
   * @param loc the location
   */
  case class TypeDefinition(
    id: Identifier,
    typeParameters: Seq[Identifier],
    body: TypeBody,
    loc: Location
  ) extends Statement {
    override def toString = {
      val typeParameterList =
        if (typeParameters.isEmpty) ""
        else typeParameters.mkString("[", ", ", "]")
      s"type $id$typeParameterList = $body"
    }
  }

  sealed abstract class TypeBody

  /**
   * A type body which is a type alias of another existing type.
   * @param typ the type to alias
   */
  case class TypeAlias(typ: Type) extends TypeBody {
    override def toString = typ.toString
  }

  /**
   * An externally defined type.
   * @param id the name of the type
   */
  case class ExternType(id: Identifier) extends TypeBody {
    override def toString = s"extern($id)"
  }

  /**
   * An input stream.
   * @param id the name of the stream
   * @param streamType the type
   * @param annotations the annotations
   * @param loc the location
   */
  case class In(id: Identifier, streamType: Type, annotations: Seq[Annotation], loc: Location) extends Statement {
    override def toString = s"in $id: $streamType"
  }

  /**
   * An output stream.
   * @param expr the expression of this stream
   * @param id the name of the output stream
   * @param annotations the annotations
   * @param loc the location
   */
  case class Out(expr: Expression, id: Identifier, annotations: Seq[Annotation], loc: Location) extends Statement {
    def name = id.name

    override def toString = s"out $expr as $name"
  }

  /**
   * An 'out *' statement.
   * @param annotations the annotations used
   * @param loc the location
   */
  case class OutAll(annotations: Seq[Annotation], loc: Location) extends Statement {
    override def toString = "out *"
  }

  def getId(statement: Tessla.Statement): Option[Tessla.Identifier] = statement match {
    case definition: Tessla.Definition => Some(definition.id)
    case in: Tessla.In                 => Some(in.id)
    case module: Tessla.Module         => Some(module.id)
    case _                             => None
  }

  /**
   * A parameter
   * @param id the name of the parameter
   * @param parameterType the type
   */
  case class Parameter(id: Identifier, parameterType: Option[Type]) extends Location.HasLoc {
    override def toString = parameterType match {
      case Some(t) => s"${id.name}: $t"
      case None    => id.name
    }

    def name: String = id.name

    def loc: Location = parameterType match {
      case Some(t) => id.loc.merge(t.loc)
      case None    => id.loc
    }
  }

  sealed abstract class Expression extends Location.HasLoc {
    def loc: Location

    def toString(inner: Boolean): String

    override def toString: String = toString(false)
  }

  /**
   * A variable.
   * @param id the name of the variable
   */
  case class Variable(id: Identifier) extends Expression {
    def loc = id.loc

    override def toString(inner: Boolean) = id.name
  }

  /**
   * Member access to a member on root scope.
   * @param member the member to access
   * @param loc the location
   */
  case class RootMemberAccess(member: Identifier, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"__root__.$member"
  }

  private val ID_PATTERN = "^[a-zA-Z0-9_]+$".r

  /**
   * Call of a macro expression
   * @param mac the macro to call
   * @param typeArgs the type arguments used
   * @param args the arguments used
   * @param loc the location
   */
  case class MacroCall(mac: Expression, typeArgs: Seq[Type], args: Seq[Argument[Expression]], loc: Location)
      extends Expression {
    override def toString(inner: Boolean) = {
      val typeArgList =
        if (typeArgs.isEmpty) ""
        else typeArgs.mkString("[", ", ", "]")

      var atomic = true
      val str = mac match {
        case Variable(id) =>
          (id.name, args) match {
            case (ID_PATTERN(), _) | (_, Seq()) => s"$mac$typeArgList(${args.mkString(", ")})"
            case ("if then", Seq(cond, thenCase)) =>
              atomic = false
              s"if $cond then $thenCase"
            case (name, Seq(PositionalArgument(arg))) => s"$name${arg.toString(inner = true)}"
            case (name, Seq(PositionalArgument(lhs), PositionalArgument(rhs))) =>
              atomic = false
              s"${lhs.toString(inner = true)} $name ${rhs.toString(inner = true)}"
            case (name, _) => s"$name$typeArgList(${args.mkString(", ")})"
          }
        case _ => s"${mac.toString(inner = true)}$typeArgList(${args.mkString(", ")})"
      }
      if (inner && !atomic) s"($str)" else str
    }
  }

  /**
   * A block expression
   * @param definitions the definitions contained by the block
   * @param expression the result expression for the block
   * @param loc the location
   */
  case class Block(definitions: Seq[Definition], expression: Expression, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"{\n${definitions.mkString("\n")}\n$expression\n}"
  }

  /**
   * A record creation literal.
   * @param members the members of this record, as a mapping from identifiers to expressions
   * @param loc the location
   */
  case class ObjectLiteral(members: Map[Identifier, Expression], loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      members.mkString("${", ", ", "}")
    }
  }

  /**
   * A lambda expression.
   * @param parameters the parameters of the lambda
   * @param headerLoc the location of the header
   * @param body the body
   * @param loc the location
   */
  case class Lambda(
    parameters: Seq[(Option[TesslaAST.RuntimeEvaluation], Parameter)],
    headerLoc: Location,
    body: Expression,
    loc: Location
  ) extends Expression {
    override def toString(inner: Boolean) = {
      val str = s"fun (${parameters.mkString(", ")}) => $body"
      if (inner) s"($str)" else str
    }
  }

  /**
   * An accessor to a record expression.
   * @param receiver the object to access a member of
   * @param member the name of the member to access
   * @param loc the location
   */
  case class MemberAccess(receiver: Expression, member: Identifier, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"${receiver.toString(inner = true)}.$member"
  }

  /**
   * A wrapper for literal values, extending them with location information.
   * @param value the literal value
   * @param loc the location
   */
  case class Literal(value: LiteralValue, loc: Location) extends Expression {
    override def toString(inner: Boolean) = value.toString
  }

  sealed abstract class LiteralValue {
    def value: Any

    override def toString = value.toString
  }

  /**
   * An integer literal.
   */
  case class IntLiteral(value: BigInt) extends LiteralValue

  /**
   * A time literal.
   * @param value the numeric time value of the literal
   * @param unit the unit associated with this literal
   */
  case class TimeLiteral(value: BigInt, unit: TimeUnit) extends LiteralValue {
    override def toString = s"$value $unit"
  }

  object TimeLiteral {
    def fromString(str: String, loc: Location): TimeLiteral = {
      val re = raw"""([0-9]+)\s*([_\p{L}][_\p{L}\p{Digit}]*)""".r
      str match {
        case re(int, unit) => TimeLiteral(int.toInt, TimeUnit.fromString(unit, loc))
        case _             => throw ParserError(s"Unable to parse time literal $str", loc)
      }
    }
  }

  /**
   * A float literal.
   */
  case class FloatLiteral(value: Double) extends LiteralValue

  /**
   * A string literal.
   */
  case class StringLiteral(value: String) extends LiteralValue {
    override def toString = s""""$value""""
  }

  abstract class Argument[T <: Location.HasLoc] extends Location.HasLoc {
    def loc: Location
  }

  /**
   * A positional argument, which is a wrapper of an expression.
   * @param expr the value of the argument
   * @tparam T the type of the argument
   */
  case class PositionalArgument[T <: Location.HasLoc](expr: T) extends Argument[T] {
    override def toString = expr.toString

    def loc = expr.loc
  }

  /**
   * A named argument.
   * @param id the name of the argument
   * @param expr the value of the argument
   * @tparam T the type of the argument
   */
  case class NamedArgument[T <: Location.HasLoc](id: Identifier, expr: T) extends Argument[T] {
    override def toString = s"$id = $expr"

    def name = id.name

    def loc = id.loc.merge(expr.loc)
  }

  sealed abstract class Type extends Location.HasLoc {
    def loc: Location
  }

  /**
   * A primitive type defined only by a name.
   * @param id the name of the type
   */
  case class SimpleType(id: Identifier) extends Type {
    def loc = id.loc

    override def toString = id.name
  }

  /**
   * A type application.
   * @param id the identifier affected by the type application
   * @param args the types to apply
   * @param loc the location
   */
  case class TypeApplication(id: Identifier, args: Seq[Type], loc: Location) extends Type {
    override def toString = s"$id[${args.mkString(", ")}]"
  }

  /**
   * A function type.
   * @param parameterTypes the types of the parameter list
   * @param returnType the return type
   * @param loc the location
   */
  case class FunctionType(
    parameterTypes: Seq[(Option[TesslaAST.RuntimeEvaluation], Type)],
    returnType: Type,
    loc: Location
  ) extends Type {
    override def toString = s"(${parameterTypes.mkString(", ")}) => $returnType]"
  }

  /**
   * An object type.
   * @param memberTypes the types of its members
   * @param loc the location
   */
  case class ObjectType(memberTypes: Map[Identifier, Type], loc: Location) extends Type {
    override def toString = {
      val members = memberTypes.toSeq.map { case (name, t) => s"$name : $t" }
      members.mkString("{", ", ", "}")
    }
  }

  /**
   * A mapping from unary operator symbols to their Tessla operator names
   */
  val unaryOperators = Map(
    "!" -> "not",
    "-" -> "negate",
    "-." -> "fnegate",
    "~" -> "bitflip"
  )

  /**
   * A mapping from binary operator symbols to their Tessla operator names
   */
  val binaryOperators = Map(
    "&&" -> "and",
    "||" -> "or",
    "==" -> "eq",
    "!=" -> "neq",
    ">" -> "gt",
    "<" -> "lt",
    ">=" -> "geq",
    "<=" -> "leq",
    ">." -> "fgt",
    "<." -> "flt",
    ">=." -> "fgeq",
    "<=." -> "fleq",
    "+" -> "add",
    "-" -> "sub",
    "*" -> "mul",
    "/" -> "div",
    "%" -> "mod",
    "&" -> "bitand",
    "|" -> "bitor",
    "^" -> "bitxor",
    "<<" -> "leftshift",
    ">>" -> "rightshift",
    "+." -> "fadd",
    "-." -> "fsub",
    "*." -> "fmul",
    "/." -> "fdiv"
  )
}
