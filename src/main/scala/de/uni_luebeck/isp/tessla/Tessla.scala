package de.uni_luebeck.isp.tessla

object Tessla {
  case class Specification(statements: Seq[Statement]) {
    override def toString = statements.mkString("\n")
  }

  case class Identifier(name: String, loc: Location) extends Location.HasLoc {
    override def toString = name
  }

  abstract sealed class Statement extends Location.HasLoc {
    def loc: Location
  }

  case class Definition( annotations: Seq[Annotation],
                         id: Identifier,
                         typeParameters: Seq[Identifier],
                         parameters: Seq[Parameter],
                         returnType: Option[Type],
                         headerLoc: Location,
                         body: Body,
                         loc: Location) extends Statement {
    override def toString = toString(objectNotation = false)

    def toString(objectNotation: Boolean) = {
      val annotationList = annotations.map("@" + _ + "\n").mkString
      val typeParameterList =
        if (typeParameters.isEmpty) ""
        else typeParameters.mkString("[", ", ", "]")
      val parameterList =
        if (parameters.isEmpty) ""
        else parameters.mkString("(", ", ", ")")
      val defString = if (objectNotation) "" else "def "
      val assign = if (objectNotation) "=" else ":="
      s"$annotationList$defString$id$typeParameterList$parameterList $assign $body"
    }
  }

  case class Annotation(id: Identifier, arguments: Seq[Argument[ConstantExpression]], loc: Location) {
    def name: String = id.name
  }

  sealed abstract class Body extends Location.HasLoc {
    def loc: Location
  }

  case class ExpressionBody(exp: Expression) extends Body {
    override def toString = exp.toString

    override def loc = exp.loc
  }

  case class BuiltInBody(id: Identifier, referenceImplementation: Option[Expression]) extends Body {
    override def toString = s"__builtin__($id)"

    override def loc = id.loc
  }

  case class Module(id: Identifier, contents: Seq[Statement], loc: Location) extends Statement {
    override def toString = contents.mkString(s"module $id {\n", "\n", "\n}")

    def name: String = id.name
  }

  case class AnnotationDefinition(id: Identifier, parameters: Seq[Parameter], loc: Location) extends Statement

  case class TypeDefinition(id: Identifier, typeParameters: Seq[Identifier], body: TypeBody, loc: Location) extends Statement {
    override def toString = {
      val typeParameterList =
        if (typeParameters.isEmpty) ""
        else typeParameters.mkString("[", ", ", "]")
      s"type $id$typeParameterList = $body"
    }
  }

  sealed abstract class TypeBody

  case class TypeAlias(typ: Type) extends TypeBody {
    override def toString = typ.toString
  }

  case class BuiltInType(id: Identifier) extends TypeBody {
    override def toString = s"__builtin__($id)"
  }

  case class In(id: Identifier, streamType: Type, annotations: Seq[Annotation], loc: Location) extends Statement {
    override def toString = s"in $id: $streamType"
  }

  case class Out(expr: Expression, id: Identifier, loc: Location) extends Statement {
    def name = id.name

    override def toString = s"out $expr as $name"
  }

  case class OutAll(loc: Location) extends Statement {
    override def toString = "out *"
  }

  case class Print(expr: Expression, loc: Location) extends Statement {
    override def toString = s"print $expr"
  }

  def getId(statement: Tessla.Statement): Option[Tessla.Identifier] = statement match {
    case definition: Tessla.Definition => Some(definition.id)
    case in: Tessla.In => Some(in.id)
    case module: Tessla.Module => Some(module.id)
    case _ => None
  }

  case class Parameter(id: Identifier, parameterType: Option[Type]) extends Location.HasLoc {
    override def toString = parameterType match {
      case Some(t) => s"${id.name}: $t"
      case None => id.name
    }

    def name: String = id.name

    def loc: Location = parameterType match {
      case Some(t) => id.loc.merge(t.loc)
      case None => id.loc
    }
  }

  sealed abstract class Expression extends Location.HasLoc {
    def loc: Location
    def toString(inner: Boolean): String
    override def toString: String = toString(false)
  }

  case class Variable(id: Identifier) extends Expression {
    def loc = id.loc
    override def toString(inner: Boolean) = id.name
  }

  case class RootMemberAccess(member: Identifier, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"__root__.$member"
  }

  private val ID_PATTERN = "^[a-zA-Z0-9_]+$".r

  case class MacroCall(mac: Expression, typeArgs: Seq[Type], args: Seq[Argument[Expression]], loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      val typeArgList =
        if (typeArgs.isEmpty) ""
        else typeArgs.mkString("[", ", ", "]")

      var atomic = true
      val str = mac match {
        case Variable(id) =>
          (id.name, args) match {
            case (ID_PATTERN(), _) | (_, Seq()) =>
              s"$mac$typeArgList(${args.mkString(", ")})"
            case ("if then", Seq(cond, thenCase)) =>
              atomic = false
              s"if $cond then $thenCase"
            case (name, Seq(PositionalArgument(arg))) =>
              s"$name${arg.toString(inner = true)}"
            case (name, Seq(PositionalArgument(lhs), PositionalArgument(rhs))) =>
              atomic = false
              s"${lhs.toString(inner = true)} $name ${rhs.toString(inner = true)}"
            case (name, _) =>
              s"$name$typeArgList(${args.mkString(", ")})"
          }
        case _ =>
          s"${mac.toString(inner = true)}$typeArgList(${args.mkString(", ")})"
      }
      if (inner && !atomic) s"($str)" else str
    }
  }

  case class StaticIfThenElse(condition: Expression, thenCase: Expression, elseCase: Expression, loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      val str = s"if $condition then $thenCase else $elseCase"
      if (inner) s"($str)" else str
    }
  }

  case class Block(definitions: Seq[Definition], expression: Expression, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"{\n${definitions.mkString("\n")}\n$expression\n}"
  }

  case class ObjectLiteral(members: Map[Identifier, Expression], loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      members.mkString("${", ", ", "}")
    }
  }

  case class Lambda(parameters: Seq[Parameter], headerLoc: Location, body: Expression, loc: Location) extends Expression {
    override def toString(inner: Boolean) = {
      val str = s"fun (${parameters.mkString(", ")}) => $body"
      if (inner) s"($str)" else str
    }
  }

  case class MemberAccess(receiver: Expression, member: Identifier, loc: Location) extends Expression {
    override def toString(inner: Boolean) = s"${receiver.toString(inner = true)}.$member"
  }

  case class Literal(value: LiteralValue, loc: Location) extends Expression {
    override def toString(inner: Boolean) = value.toString
  }

  sealed abstract class LiteralValue {
    def value: Any
    override def toString = value.toString
  }

  case class IntLiteral(value: BigInt) extends LiteralValue

  case class TimeLiteral(value: BigInt, unit: TimeUnit) extends LiteralValue {
    override def toString = s"$value $unit"
  }

  case class FloatLiteral(value: Double) extends LiteralValue

  case class StringLiteral(value: String) extends LiteralValue {
    override def toString = s""""$value""""
  }

  sealed abstract class ConstantExpression extends Location.HasLoc

  object ConstantExpression {
    case class Literal(value: LiteralValue, loc: Location) extends ConstantExpression {
      override def toString = value.toString
    }

    case class Object(members: Map[Identifier, ConstantExpression], loc: Location) extends ConstantExpression {
      override def toString = {
        members.map {
          case (name, value) => s"$name = $value"
        }.mkString("{", ", ", "}")
      }
    }
  }

  abstract class Argument[T <: Location.HasLoc] extends Location.HasLoc {
    def loc: Location
  }

  case class PositionalArgument[T <: Location.HasLoc](expr: T) extends Argument[T] {
    override def toString = expr.toString
    def loc = expr.loc
  }

  case class NamedArgument[T <: Location.HasLoc](id: Identifier, expr: T) extends Argument[T] {
    override def toString = s"$id = $expr"
    def name = id.name
    def loc = id.loc.merge(expr.loc)
  }

  sealed abstract class Type extends Location.HasLoc {
    def loc: Location
  }

  case class SimpleType(id: Identifier) extends Type {
    def loc = id.loc
    override def toString = id.name
  }

  case class TypeApplication(id: Identifier, args: Seq[Type], loc: Location) extends Type {
    override def toString = s"$id[${args.mkString(", ")}]"
  }

  case class FunctionType(parameterTypes: Seq[Type], returnType: Type, loc: Location) extends Type {
    override def toString = s"(${parameterTypes.mkString(", ")}) => $returnType]"
  }

  case class ObjectType(memberTypes: Map[Identifier, Type], isOpen: Boolean, loc: Location) extends Type {
    override def toString = {
      var members = memberTypes.toSeq.map {case (name, t) => s"$name : $t"}
      if (isOpen) {
        members :+= ".."
      }
      members.mkString("{", ", ", "}")
    }
  }

  val unaryOperators = Map(
    "!" -> "__not__",
    "-" -> "__negate__",
    "-." -> "__fnegate__",
    "~" -> "__bitflip__"
  )

  val binaryOperators = Map(
    "&&" -> "__and__",
    "||" -> "__or__",
    "==" -> "__eq__",
    "!=" -> "__neq__",
    ">" -> "__gt__",
    "<" -> "__lt__",
    ">=" -> "__geq__",
    "<=" -> "__leq__",
    ">." -> "__fgt__",
    "<." -> "__flt__",
    ">=." -> "__fgeq__",
    "<=." -> "__fleq__",
    "+" -> "__add__",
    "-" -> "__sub__",
    "*" -> "__mul__",
    "/" -> "__div__",
    "%" -> "__mod__",
    "&" -> "__bitand__",
    "|" -> "__bitor__",
    "^" -> "__bitxor__",
    "<<" -> "__leftshift__",
    ">>" -> "__rightshift__",
    "+." -> "__fadd__",
    "-." -> "__fsub__",
    "*." -> "__fmul__",
    "/." -> "__fdiv__"
  )
}