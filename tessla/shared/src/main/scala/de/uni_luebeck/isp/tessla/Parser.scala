package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.compacom.Parsers
import de.uni_luebeck.isp.tessla.AST._
import de.uni_luebeck.isp.tessla.Tokens._

object Parser extends Parsers {
  val tokens = Tokens
  
  case class Ctx(var definedNames: Set[String] = Set(), var macroArguments: Set[String] = Set()) {
    def completions = definedNames | macroArguments
    def typeCompletions = Set("int", "bool")
    def inMacro = !macroArguments.isEmpty
  }
  
  private def updateLoc[Result <: Locatable](parser: Parser[Result]): Parser[Result] = {
    parser ^^! ((l, r) => {r.updateLoc(l); r})
  }
  
  def spec(ctx: Ctx = Ctx()): Parser[Spec] = statement(ctx).* ^^ (s => Spec(statements = s))
  
  def statement(ctx: Ctx): Parser[Statement] = updateLoc(defOrMacroDef(ctx) | out(ctx))
  
  def defOrMacroDef(ctx: Ctx): Parser[Statement] =
    token(DEFINE) ~> (matchTokenAlt(Set("value name", "macro name"), Set("<value-name>", "<macro-name>")) {
      case ID(name) => 
          (token(DEFINE_AS) ~> defBody(ctx, name)) |
          ((token(OF_TYPE) ~> typ(ctx) <~ token(DEFINE_AS)) ~^ (t => defBody(ctx, name) ^^ {case Def(name, d) => Def(name, TypeAscr(d, t))})) |
          macroArgsAndBody(ctx, name) } ~^ identity)
  
  def defBody(ctx: Ctx, name: String): Parser[Def] = term(ctx) ^^ (e => {ctx.definedNames += name; Def(name, e)})
  
  def macroArgsAndBody(ctx: Ctx, name: String): Parser[MacroDef] =
    (token(LPAREN) ~>
      rep1sep(matchToken("macro argument name", Set("<macro-argument-name>")) {case ID(name) => name}, token(COMMA))
      <~ token(RPAREN) <~ token(DEFINE_AS)) ~^
      (macroArgs => term(ctx.copy(macroArguments = macroArgs.toSet)) ^^ (rhs => MacroDef(name, macroArgs, rhs)))
  
  def term(ctx: Ctx): Parser[Term] = baseTerm(ctx) ~^ (lhs => (token(OF_TYPE) ~> typ(ctx) ^^ (rhs => TypeAscr(lhs, rhs))) | success(lhs))
      
  def baseTerm(ctx: Ctx): Parser[Term] = namedTermOrApp(ctx) | integralTerm(ctx) | (token(LPAREN) ~> term(ctx) <~ token(RPAREN))
  
  def integralTerm(ctx: Ctx): Parser[Const] = matchToken("integer", Set()) {case LIT_INT(x) => Const(IntegralConstant(x))}
  
  def namedTermOrApp(ctx: Ctx): Parser[Term] = updateLoc(
      matchTokenAlt(Set("defined name", "function name") | (if (ctx.inMacro) Set("macro argument") else Set()), ctx.completions) {
        case ID(name) => functionCall(ctx, name) | success(UnresolvedTerm(name)) } ~^ identity)
   
  def functionCall(ctx: Ctx, name: String): Parser[App] =
    token(LPAREN) ~> (rep1sep(functionArg(ctx), token(COMMA)) ^^ buildApp(name)) <~ token(RPAREN)
    
  private def buildApp(name: String)(args: List[Either[Def, Term]]): App = {
    App(UnresolvedFunction(name), args.collect({case Right(t) => t}), args.collect({case Left(t) => t}))    
  }
  
  def functionArg(ctx: Ctx): Parser[Either[Def, Term]] = term(ctx) ~^ (lhs => lhs match {
    case UnresolvedTerm(name, _) => (token(DEFINE_AS) ~> term(ctx) ^^ (rhs => Left(Def(name, rhs)))) | success(Right(lhs))
    case _ => success(Right(lhs))})
  
  def out(ctx: Ctx): Parser[Out] = updateLoc(
    (token(OUT) ~> matchToken("output name", Set("<value-name>")) {case ID(name) => Out(name)}))
    
  def typ(ctx: Ctx): Parser[Type] = updateLoc(matchToken("type name", ctx.typeCompletions) {case ID(name) => UnresolvedPrimitiveType(name)})
}