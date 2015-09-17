package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom._
import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.Tokens._
import de.uni_luebeck.isp.tessla.Compiler.Pass
import scala.util.Try

object Parser extends Parsers with Pass {
  case class ParserError(val parserFailure: Failure) extends Exception {
    override def toString = "ParserError(" + parserFailure + ")"
  }

  def applyPass(compiler: Compiler, state: Compiler.State): Try[Compiler.State] = {
    state match {
      case Compiler.Source(source) => parseAll(spec(), source) match {
        case Success(_, spec, _, _) => scala.util.Success(Compiler.Tree(spec))
        case fail: Failure => scala.util.Failure(ParserError(fail))
      }
      case _ => scala.util.Failure[Compiler.State](Compiler.UnexpectedCompilerState)
    }
  }
  
  val tokens = Tokens
  override val tokenizer = Tokenizer
  
  case class Ctx(var definedNames: Set[String] = Set(), var macroArguments: Set[String] = Set()) {
    def completions = (definedNames | macroArguments).map(x => x : Suggestion)
    def typeCompletions = Set("int" : Suggestion, "bool" : Suggestion)
    def inMacro = !macroArguments.isEmpty
  }
  
  private def updateLoc[Result <: Locatable](parser: Parser[Result]): Parser[Result] = {
    parser ^^! ((l, r) => {r.updateLoc(l); r})
  }
  
  def spec(ctx: Ctx = Ctx()): Parser[Spec] = statement(ctx).* ^^ (s => Spec(statements = s))
  
  def statement(ctx: Ctx): Parser[Statement] = updateLoc(defOrMacroDef(ctx) | out(ctx))
  
  def defOrMacroDef(ctx: Ctx): Parser[Statement] =
    token(DEFINE) ~> (matchTokenAlt(Set("value name", "macro name"), Set("<value-name>", "<macro-name>")) {
      case WithLocation(_, ID(name)) => 
          (token(DEFINE_AS) ~> defBody(ctx, name)) |
          ((token(OF_TYPE) ~> typ(ctx) <~ token(DEFINE_AS)) ~^ (t => defBody(ctx, name) ^^ {case Def(name, d) => Def(name, TreeTerm(TypeAscr(d, t)))})) |
          macroArgsAndBody(ctx, name) } ~^ identity)
  
  def defBody(ctx: Ctx, name: String): Parser[Def] = term(ctx) ^^ (e => {ctx.definedNames += name; Def(name, TreeTerm(e))})
  
  def macroArgsAndBody(ctx: Ctx, name: String): Parser[MacroDef] =
    (token(LPAREN) ~>
      rep1sep(matchToken("macro argument name", Set("<macro-argument-name>")) {case WithLocation(_, ID(name)) => name}, token(COMMA))
      <~ token(RPAREN) <~ token(DEFINE_AS)) ~^
      (macroArgs => term(ctx.copy(macroArguments = macroArgs.toSet)) ^^ (rhs => MacroDef(name, macroArgs, TreeTerm(rhs))))
  
  def term(ctx: Ctx): Parser[Term[TreeTerm]] = baseTerm(ctx) ~^ (lhs => (token(OF_TYPE) ~> typ(ctx) ^^ (rhs => TypeAscr(TreeTerm(lhs), rhs))) | success(lhs))
      
  def baseTerm(ctx: Ctx): Parser[Term[TreeTerm]] = namedTermOrApp(ctx) | integralTerm(ctx) | (token(LPAREN) ~> term(ctx) <~ token(RPAREN))
  
  def integralTerm(ctx: Ctx): Parser[Const] = matchToken("integer", Set()) {case WithLocation(_, LIT_INT(x)) => Const(IntegralConstant(x))}
  
  def namedTermOrApp(ctx: Ctx): Parser[Term[TreeTerm]] = updateLoc(
      matchTokenAlt(Set("defined name", "function name") | (if (ctx.inMacro) Set("macro argument") else Set()), ctx.completions) {
        case WithLocation(_, ID(name)) => functionCall(ctx, name) | success(UnresolvedTerm(name)) } ~^ identity _)
   
  def functionCall(ctx: Ctx, name: String): Parser[App[TreeTerm]] =
    token(LPAREN) ~> (rep1sep(functionArg(ctx), token(COMMA)) ^^ buildApp(name)) <~ token(RPAREN)
    
  private def buildApp(name: String)(args: List[Either[Def, Term[TreeTerm]]]): App[TreeTerm] = {
    App(UnresolvedFunction(name), args.collect({case Right(t) => TreeTerm(t)}), args.collect({case Left(Def(name, t)) => NamedArg(name, t)}))    
  }
  
  def functionArg(ctx: Ctx): Parser[Either[Def, Term[TreeTerm]]] = term(ctx) ~^ (lhs => lhs match {
    case UnresolvedTerm(name, _) => (token(DEFINE_AS) ~> term(ctx) ^^ (rhs => Left(Def(name, TreeTerm(rhs))))) | success(Right(lhs))
    case _ => success(Right(lhs))})
  
  def out(ctx: Ctx): Parser[Out] = updateLoc(
    (token(OUT) ~> matchToken("output name", Set("<value-name>")) {case WithLocation(_, ID(name)) => Out(name)}))
    
  def typ(ctx: Ctx): Parser[Type] = streamType(ctx) | updateLoc(matchToken("type name", ctx.typeCompletions) {case WithLocation(_, ID(name)) => UnresolvedPrimitiveType(name)})

  def streamType(ctx: Ctx): Parser[Type] = token(PERCENT) ~> typ(ctx) ^^ (t => StreamType(t))
}