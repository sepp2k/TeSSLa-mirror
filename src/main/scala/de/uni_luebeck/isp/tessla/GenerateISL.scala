package de.uni_luebeck.isp.tessla

object GenerateISL {
  sealed abstract class IslAst {
    def toStringSeq: Seq[String] = this match {
      case CondAst(cond, thenBody, elseBody) =>
        cond match {
          case TrueCond => thenBody.flatMap{ p => p.toStringSeq }
          case StringCond(s) => (s"if $s then " +: thenBody.flatMap{ p => p.toStringSeq }) ++
            (elseBody match {
              case Seq() => Seq("fi")
              case _ => ("else" +: elseBody.flatMap{ p => p.toStringSeq }) ++ Seq("fi")
            })
        }
      case LogAst(log) => if(!log.matches("^\\s*$"))
        "log" +: Seq(log)
      else
        Seq()
    }
    def ++(other:IslAst) = (this, other) match {
      case (CondAst(TrueCond,body1,_), CondAst(TrueCond,body2,_)) => CondAst(TrueCond, body1 ++ body2, Seq())
      case (CondAst(TrueCond,body1,_), _) => CondAst(TrueCond, other +: body1, Seq())
      case (_, CondAst(TrueCond,body2,_)) => CondAst(TrueCond, other +: body2, Seq())
      case (_, _) => CondAst(TrueCond, Seq(this, other), Seq())
    }
    def insertOnLog(l:LogAst):IslAst = this match {
      case ast :CondAst => ast.insertOnLog(l)
      case ast :LogAst => ast.insertOnLog(l)
      case _ => l
    }
  }
  sealed abstract class IslCond
  case class StringCond(string:String) extends IslCond
  case object TrueCond extends IslCond
  private def insertInBody(body:Seq[IslAst], l:LogAst): Seq[IslAst] ={
    val bodyLog = body.find {
      case LogAst(_) => true
      case _ => false
    }
    if (bodyLog.isEmpty) {
      body.map {
        case c: CondAst => c.insertOnLog(l)
        case other => other
      }
    } else {
      l +: body
    }
  }
  case class CondAst(cond:IslCond, thenBody:Seq[IslAst], elseBody:Seq[IslAst]) extends IslAst {
    override def insertOnLog(l:LogAst): IslAst = this.cond match {
      case TrueCond =>
        CondAst(this.cond, l +: this.thenBody, this.elseBody)
      case _ =>
        CondAst(this.cond, insertInBody(this.thenBody, l), insertInBody(elseBody, l))
    }
  }
  case class LogAst(log:String) extends IslAst {
    override def insertOnLog(l:LogAst): IslAst = CondAst(TrueCond, Seq(this, l), Seq())
  }
  def generateOsl(tesslaFile:String): Unit = {
    val ast = new TesslaParser().translateSpec(TesslaSource.fromFile(tesslaFile))
    println(generateForSpec(ast))
  }

  def generateForSpec(spec: Tessla.Specification): String = {
    spec.statements.map(generateForStatement).reduce(_++_).toStringSeq.filter(_.nonEmpty).mkString("\n")
  }

  def generateForStatement(statement: Tessla.Statement): IslAst= {
    statement match {
      case d: Tessla.Definition =>
        generateForExpr(d.body)
      case o: Tessla.Out =>
        generateForExpr(o.expr)
      case _ =>
        LogAst("")
    }
  }

  def generateForExpr(expr: Tessla.Expression): IslAst = {
    expr match {
      case call: Tessla.MacroCall =>
        generateForMacro(call.macroID, call.args) ++ call.args.map {
            case Tessla.NamedArgument(_, argExpr) =>
              generateForExpr(argExpr)
            case Tessla.PositionalArgument(argExpr) =>
              generateForExpr(argExpr)
          }.reduceLeft(_++_)
      case block: Tessla.Block =>
        generateForExpr(block.expression) ++ block.definitions.map{ d =>
          generateForExpr(d.body)
        }.reduce(_++_)
      case Tessla.Variable(id) => LogAst(generateForVariable(id))
      case _ =>
        LogAst("")
    }
  }

  def generateForVariable(id: Tessla.Identifier):String = id.name match {
    case "thread_id" => "thread_id"
    case "lines" => "lines"
    case "columns" => "columns"
    case "functions" => "functions"
    case "opcodes" => "opcodes"
    case "function_calls" => "function_calls"
    case "operands" => "operands"
    case "line_reached" => "line_reached"
    case _ => ""
  }
  def getIntValue(args: Seq[Tessla.Argument]): BigInt = args match {
    case Seq(Tessla.PositionalArgument(Tessla.Literal(Tessla.IntLiteral(value), _))) => value
    case Seq(Tessla.NamedArgument(_, Tessla.Literal(Tessla.IntLiteral(value), _))) => value
    case _ => null
  }
  def getStringValue(args: Seq[Tessla.Argument]): String = args match {
    case Seq(Tessla.PositionalArgument(Tessla.Literal(Tessla.StringLiteral(value), _))) => value
    case Seq(Tessla.NamedArgument(_, Tessla.Literal(Tessla.StringLiteral(value), _))) => value
    case _ => null
  }
  def generateForMacro(id: Tessla.Identifier, args: Seq[Tessla.Argument] ): IslAst = {
    id match {
      case Tessla.Identifier("code_line_exec",_) =>
        getIntValue(args) match {
          case null =>
            LogAst("line_reached")
          case value =>
            CondAst(StringCond(s"""[lines:$value]"""), Seq(LogAst("line_reached")), Seq())
        }
      case Tessla.Identifier("function_call",_) =>
        getStringValue(args) match {
          // This wont work because we are already in that function when functions evaluates
          case null =>
            LogAst("function_calls")
          case value =>
            CondAst(StringCond(s"""[function_calls:"$value"]"""), Seq(LogAst("function_calls")), Seq())
        }
      case Tessla.Identifier("function_return",_) =>
        getStringValue(args) match {
          case null =>
            LogAst("functions")
          case value =>
            CondAst(StringCond(s"""&([functions:"$value"], [opcodes: "ret"])"""), Seq(LogAst("functions"), LogAst("opcodes")), Seq())
        }
      case Tessla.Identifier("runtime",_) =>
        getStringValue(args) match {
          case value =>
            CondAst( TrueCond,
            Seq(CondAst(StringCond(s"""&([functions:"$value"], [opcodes: "ret"])"""), Seq(LogAst("functions"), LogAst("opcodes")), Seq()),
              LogAst("function_calls")),
              Seq())
          // why is there no default case?
        }
      case Tessla.Identifier("store_exec",_) =>
        getIntValue(args) match {
          case value =>
            CondAst(StringCond(s"""if &([lines: $value], [opcodes: "store"])"""), Seq(LogAst("line"), LogAst("opcode")), Seq())
        }
      case Tessla.Identifier("load_exec",_) =>
        getIntValue(args) match {
          case value =>
            CondAst(StringCond(s"""if &([lines: $value], [opcodes: "load"])"""), Seq(LogAst("line"), LogAst("opcode")), Seq())
        }
      case Tessla.Identifier("thread_id_on",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(call: Tessla.MacroCall)) =>
            generateForMacro(call.macroID, call.args).insertOnLog(LogAst("thread_id"))
          case Seq(Tessla.NamedArgument(_, call: Tessla.MacroCall)) =>
            generateForMacro(call.macroID, call.args).insertOnLog(LogAst("thread_id"))
        }
      case _ =>
        LogAst("")
    }
  }
}
