package de.uni_luebeck.isp.tessla


object GenerateISL {
  sealed abstract class IslAst {
    def toStringSeq(): Seq[String] = this match {
      case CondAst(cond, thenBody, elseBody) => {
        cond match {
          case TrueCond => thenBody.flatMap{ p => p.toStringSeq() }
          case StringCond(s) => (s"if ${s} then " +: thenBody.flatMap{ p => p.toStringSeq() }) ++
            (elseBody match {
              case Seq() => Seq("fi")
              case _ => ("else" +: elseBody.flatMap{ p => p.toStringSeq() }) ++ Seq("fi")
            })
        }
      }
      case LogAst(log) => Seq(log)
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

  def generateForSpec(spec: Tessla.Spec): String = {
    spec.statements.map(generateForStatement).reduce(_++_).toStringSeq().mkString("\n")
  }

  def generateForStatement(statement: Tessla.Statement): IslAst= {
    statement match {
      case Tessla.Definition(_, _, _, body, _) =>
        generateForExpr(body)
      case Tessla.Out(expr, _, _) =>
        generateForExpr(expr)
      case _ =>
        LogAst("")
    }
  }

  def generateForExpr(expr: Tessla.Expression): IslAst = {
    expr match {
      case Tessla.MacroCall(id, args, _) =>
        generateForMacro(id, args) ++ args.map {
            case Tessla.NamedArgument(_, argExpr) =>
              generateForExpr(argExpr)
            case Tessla.PositionalArgument(argExpr) =>
              generateForExpr(argExpr)
          }.reduceLeft(_++_)
      case Tessla.TypeAssertion(exprArg, _) =>
        generateForExpr(exprArg)
      case Tessla.Block(defs, exprArg,_) =>
        generateForExpr(exprArg) ++ defs.map{
          case Tessla.Definition(_, _, _, body, _) =>
            generateForExpr(body)
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
  def generateForMacro(id: Tessla.Identifier, args: Seq[Tessla.Argument] ): IslAst = {
    id match {
      case Tessla.Identifier("code_line_exec",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(Tessla.IntLiteral(value, _))) =>
            CondAst(StringCond(s"[line_reached:${value}]"), Seq(LogAst("line_reached")), Seq())
          case _ =>
            LogAst("line_reached")
        }
      case Tessla.Identifier("function_call",_) =>
        args match {
          // This wont work because we are already in that function when functions evaluates
          //case Seq(Tessla.PositionalArgument(Tessla.StringLiteral(value, _))) =>
          //  Seq(s"if [functions:${value}] then function_call fi")
          case _ =>
            LogAst("function_call")
        }
      case Tessla.Identifier("function_return",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(Tessla.StringLiteral(value, _))) =>
            CondAst(StringCond(s"&([functions:${value}], [opcodes: ret])"), Seq(LogAst("functions"), LogAst("opcodes")), Seq())
          case _ =>
            LogAst("functions")
        }
      case Tessla.Identifier("runtime",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(Tessla.StringLiteral(value, _))) =>
            CondAst( TrueCond,
            Seq(CondAst(StringCond(s"&([functions:${value}], [opcodes: ret]"), Seq(LogAst("functions"), LogAst("opcodes")), Seq()),
              LogAst("function_calls")),
              Seq())

        }
      case Tessla.Identifier("store_exec",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(Tessla.IntLiteral(value, _))) =>
            CondAst(StringCond(s"if &([lines: ${value}], [opcodes: store])"), Seq(LogAst("line"), LogAst("opcode")), Seq())
        }
      case Tessla.Identifier("load_exec",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(Tessla.IntLiteral(value, _))) =>
            CondAst(StringCond(s"if &([lines: ${value}], [opcodes: load])"), Seq(LogAst("line"), LogAst("opcode")), Seq())
        }
      case Tessla.Identifier("thread_id_on",_) =>
        args match {
          case Seq(Tessla.PositionalArgument(Tessla.MacroCall(macroID, macroArgs, _))) =>
            generateForMacro(macroID, macroArgs).insertOnLog(LogAst("thread_id"))
        }
      case _ =>
        LogAst("")
    }
  }
}
