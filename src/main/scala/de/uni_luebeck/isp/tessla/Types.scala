package de.uni_luebeck.isp.tessla

object Types {
  sealed abstract class Type
  sealed abstract class ValueType extends Type
  case object Int extends ValueType
  case object String extends ValueType
  case object Bool extends ValueType
  case object Unit extends ValueType
  case object Nothing extends ValueType {
    override def toString = "?"
  }
  final case class Stream(elementType: ValueType) extends Type {
    override def toString = s"Events<$elementType>"
  }

  def fromAst(ast: Ast.Type): Type = ast match {
    case Ast.TypeName(Ast.Identifier("Int", _)) => Int
    case Ast.TypeName(Ast.Identifier("String", _)) => String
    case Ast.TypeName(Ast.Identifier("Bool", _)) => Bool
    case Ast.TypeName(Ast.Identifier("Unit", _)) => Unit
    case Ast.TypeName(Ast.Identifier(name, loc)) => throw UnknownType(name, loc)
    case Ast.TypeApp(Ast.Identifier("Events", _), Seq(elementType), loc) =>
      fromAst(elementType) match {
        case Stream(_) => throw StreamOfStreams(loc)
        case t: ValueType => Stream(t)
      }
    case Ast.TypeApp(name, _, _) => throw UnknownType(name.name, name.loc)
  }

  private def requireTypeOption[T <: Type](expected: T, actual: T): Option[T] = (expected, actual) match {
    case (Nothing, _: ValueType) => Some(actual)
    case (_: ValueType, Nothing) => Some(expected)
    case (Stream(t1), Stream(t2)) =>
      requireTypeOption(t1, t2).map(Stream).asInstanceOf[Option[T]]
    case _ =>
      if (expected == actual) Some(actual)
      else None
  }

  def requireType[T <: Type](expected: T, actual: T, loc: Location): T = {
    requireTypeOption(expected, actual).getOrElse {
      throw TypeMismatch(expected, actual, loc)
    }
  }

  case class TypeMismatch(expected: Type, found: Type, loc: Location) extends CompilationError {
    override def message = s"Type mismatch: Expected $expected, found $found."
  }

  case class TypeArityMismatch(name: String, expected: Int, actual: Int, loc: Location) extends CompilationError {
    def message = s"Wrong number of type arguments for $name. Expected: $expected, actual: $actual"
  }
  case class UnknownType(name: String, loc: Location) extends CompilationError {
    def message = s"Unknown type: $name"
  }

  case class StreamOfStreams(loc: Location) extends CompilationError {
    def message = "Streams may not contain other streams"
  }
}
