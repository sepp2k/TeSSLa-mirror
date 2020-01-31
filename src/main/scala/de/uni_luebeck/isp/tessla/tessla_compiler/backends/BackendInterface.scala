package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode._
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.Errors

/**
  * Abstract base class for the translation from IntermediateCode to real source code
  * @param sourceTemplate Resource path of the template code, where generated code is inserted
  */
abstract class BackendInterface(sourceTemplate: String) extends TranslationPhase[SourceListing, String] {

  var variables : Map[String, (ImpLanType, Option[ImpLanExpr])] = Map()

  def translate(listing: SourceListing) : Result[String] = {
    var warnings = Seq()

    val nonStaticVars = getVariableMap(listing.tsGenSource.concat(listing.stepSource).concat(listing.inputProcessing)) ++
      Map("inputStream" -> (StringType, scala.Some(StringValue(""))),
      "value" -> (StringType, scala.Some(StringValue(""))),
      "currTs" -> (LongType, scala.Some(LongValue(0))))

    val staticVars = getVariableMap(listing.staticSource)

    variables = nonStaticVars ++ staticVars

    val source =  scala.io.Source.fromResource(sourceTemplate).mkString
    val rewrittenSource = source.replaceAllLiterally("//VARDEF", generateVariableDeclarations(nonStaticVars).mkString("\n"))
        .replaceAllLiterally("//TRIGGER", generateCode(listing.tsGenSource))
        .replaceAllLiterally("//STEP", generateCode(listing.stepSource))
        .replaceAllLiterally("//INPUTPROCESSING", generateCode(listing.inputProcessing))
        .replaceAllLiterally("//STATIC", generateVariableDeclarations(staticVars).mkString("\n") + "\n\n" + generateCode(listing.staticSource))
    Success(rewrittenSource, warnings)
  }

  def generateVariableDeclarations(vars: Map[String, (ImpLanType, Option[ImpLanExpr])]) : Seq[String]

  def generateCode(stmts: Seq[ImpLanStmt]) : String

  def getVariableMap(stmts: Seq[ImpLanStmt]) : Map[String, (ImpLanType, Option[ImpLanExpr])] = {

    def extractAssignments(stmt: ImpLanStmt) : Seq[(String, ImpLanType, Option[ImpLanExpr])] = stmt match {
      case Assignment(lhs, _, defVal, typ) => Seq((lhs.name, typ, defVal))
      case FinalAssignment(lhs, defVal, typ) => Seq((lhs.name, typ, scala.Some(defVal)))
      case If(_, stmts, elseStmts) => stmts.concat(elseStmts).flatMap(extractAssignments)
      case _ => Seq()
    }

    val varDefs = stmts.flatMap(extractAssignments).distinct
    val duplicates = varDefs.groupBy{case (n, _, _) => n}.collect{case (x, List(_,_,_*)) => x}

    if (duplicates.size != 0) {
      throw new Errors.TranslationError(s"Variable(s) with unsound type/default information: ${duplicates.mkString(", ")}")
    }

    varDefs.map{case (name, typ, default) => (name, (typ, default))}.toMap
  }
}
