package de.uni_luebeck.isp.tessla.tessla_compiler.backends

import de.uni_luebeck.isp.tessla.TranslationPhase
import de.uni_luebeck.isp.tessla.tessla_compiler.IntermediateCode.{Assignment, FinalAssignment, If, ImpLanStmt, ImpLanType, ImpLanVal, LongType, LongValue, SourceListing, StringType, StringValue}
import de.uni_luebeck.isp.tessla.TranslationPhase.{Result, Success}
import de.uni_luebeck.isp.tessla.tessla_compiler.Errors

/**
  * Abstract base class for the translation from IntermediateCode to real source code
  * @param sourceTemplate Resource path of the template code, where generated code is inserted
  */
abstract class BackendInterface(sourceTemplate: String) extends TranslationPhase[SourceListing, String] {

  var variables : Map[String, (ImpLanType, ImpLanVal)] = Map()

  def translate(listing: SourceListing) : Result[String] = {
    var warnings = Seq()
    variables = getVariableMap(listing) ++ Map("inputStream" -> (StringType, StringValue("")),
                                               "value" -> (StringType, StringValue("")),
                                               "currTs" -> (LongType, LongValue(0)))

    val source =  scala.io.Source.fromResource(sourceTemplate).mkString
    val rewrittenSource = source.replaceAllLiterally("//VARDEF", generateVariableDeclarations().mkString("\n"))
        .replaceAllLiterally("//TRIGGER", generateCode(listing.tsGenSource))
        .replaceAllLiterally("//STEP", generateCode(listing.stepSource))
        .replaceAllLiterally("//INPUTPROCESSING", generateCode(listing.inputProcessing))
    Success(rewrittenSource, warnings)
  }

  def generateVariableDeclarations() : Seq[String]

  def generateCode(stmts: Seq[ImpLanStmt]) : String

  def getVariableMap(listing: SourceListing) : Map[String, (ImpLanType, ImpLanVal)] = {

    def extractAssignments(stmt: ImpLanStmt) : Seq[(String, ImpLanType, ImpLanVal)] = stmt match {
      case Assignment(lhs, _, defVal, typ) => Seq((lhs.name, typ, defVal))
      case FinalAssignment(lhs, defVal, typ) => Seq((lhs.name, typ, defVal))
      case If(_, stmts, elseStmts) => stmts.union(elseStmts).flatMap(extractAssignments)
      case _ => Seq()
    }

    val varDefs = listing.tsGenSource.union(listing.stepSource).union(listing.inputProcessing).
                  flatMap(extractAssignments).distinct
    val duplicates = varDefs.groupBy{case (n, _, _) => n}.collect{case (x, List(_,_,_*)) => x}

    if (duplicates.size != 0) {
      throw new Errors.TranslationError(s"Variable(s) with unsound type/default information: ${duplicates.mkString(", ")}")
    }

    varDefs.map{case (name, typ, default) => (name, (typ, default))}.toMap
  }
}
