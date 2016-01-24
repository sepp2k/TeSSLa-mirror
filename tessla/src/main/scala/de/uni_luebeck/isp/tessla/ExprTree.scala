package de.uni_luebeck.isp.tessla

abstract class ArgName

case class Pos(pos: Integer) extends ArgName

// TODO right now the loc of the name is lost, but otherwise using this as key is problematic
case class Named(name: String) extends ArgName

case class ExprTree(fn: ExprTreeFn, args: Map[ArgName, ExprTree], loc: NestedLoc)

abstract class ExprTreeFn

case class NamedFn(name: String, loc: NestedLoc) extends ExprTreeFn
case class TypeAscrFn(`type`: Type, loc: NestedLoc) extends ExprTreeFn
case class LiteralFn(value: LiteralValue, loc: NestedLoc) extends ExprTreeFn

abstract class LiteralValue
case class IntLiteral(value: BigInt) extends LiteralValue
case class StringLiteral(value: String) extends LiteralValue

case class Definitions(
  streamDefs: Map[String, StreamDef],
  macroDefs: Map[String, MacroDef]
)

case class MacroDef(
  args: Seq[(String, NestedLoc)],
  streamDef: StreamDef
)

case class StreamDef(
  name: String,
  loc: NestedLoc,
  expr: ExprTree
)