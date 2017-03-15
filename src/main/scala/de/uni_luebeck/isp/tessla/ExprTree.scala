package de.uni_luebeck.isp.tessla

abstract class ArgName

case class Pos(pos: Integer) extends ArgName

// TODO right now the loc of the name is lost, but otherwise using this as key is problematic
case class Named(name: String) extends ArgName

case class ExprTree(fn: ExprTreeFn, args: Map[ArgName, ExprTree], loc: NestedLoc) {
  private lazy val ppArgs: String = {
    val posArgs = args.toList.collect {
      case (Pos(idx), arg) => (idx, arg.toString)
    }.sorted.map(_._2)

    val kwArgs = args.collect {
      case (Named(name), arg) => s"$name: $arg"
    }
    (posArgs ++ kwArgs).mkString(", ")
  }

  override def toString = fn.prettyPrint(ppArgs)
}

sealed abstract class ExprTreeFn {
  def prettyPrint(argString: =>String): String
}

case class NamedFn(name: String, loc: NestedLoc) extends ExprTreeFn {
  override def prettyPrint(argString: => String) =  {
    if(argString.isEmpty) name
    else s"$name( $argString )"
  }
}

case class InputFn(name: String, `type`: Type, loc: NestedLoc) extends ExprTreeFn {
  override def prettyPrint(_argString: => String) =  s"input($name)"
}
case class TypeAscrFn(`type`: Type, loc: NestedLoc) extends ExprTreeFn {
  override def prettyPrint(argString: => String) =  s"($argString : ${`type`})"
}
case class LiteralFn(value: LiteralValue, loc: NestedLoc) extends ExprTreeFn {
  override def prettyPrint(_argString: => String) = value.value.toString
}

sealed abstract class LiteralValue {
  val value: Any
}

case class IntLiteral(value: BigInt) extends LiteralValue
case class StringLiteral(value: String) extends LiteralValue
case class BoolLiteral(value: Boolean) extends LiteralValue
case class FloatLiteral(value: BigDecimal) extends LiteralValue

case class Definitions(
  streamDefs: Map[String, StreamDef],
  macroDefs: Map[String, MacroDef],
  outStreams: Map[String, OutDef]
) {
  override def toString = {
    val ppStreams = streamDefs.map {
      case (name, streamDef) =>
        s"define $name := ${streamDef.expr}\n"
    }
    val ppMacros = macroDefs.map {
      case (name, macroDef) =>
        val args = macroDef.args.map(_._1).mkString(", ")
        s"define $name($args) := ${macroDef.streamDef.expr}\n"
    }
    val ppOuts = outStreams.map {
      case (name, _) =>
        s"out $name\n"
    }
    (ppStreams ++ ppMacros ++ ppOuts).mkString
  }
}

case class MacroDef(
  args: Seq[(String, NestedLoc)],
  streamDef: StreamDef
)

case class StreamDef(
  name: String,
  loc: NestedLoc,
  expr: ExprTree
)

case class OutDef(
  name: String,
  loc: NestedLoc
)