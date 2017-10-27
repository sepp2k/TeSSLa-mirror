package de.uni_luebeck.isp.tessla

import de.uni_luebeck.isp.tessla.Warnings.ConflictingOut

class Scoper extends TranslationPhase[Tessla.Specification, ScopedTessla.Specification] {
  override def translateSpec(spec: Tessla.Specification) = {
    translateStatements(spec.statements, ScopedTessla.Specification(Map(), Seq(), outAllLocation = None))
  }

  def translateStatements(statements: Seq[Tessla.Statement], result: ScopedTessla.Specification): ScopedTessla.Specification = {
    statements match {
      case Nil => result
      case (outAll : Tessla.OutAll) :: statements =>
        if(result.outAll) warn(ConflictingOut(outAll.loc, previous = result.outAllLocation.get))
        translateStatements(statements, result.copy(outAllLocation = Some(outAll.loc)))
      case (out: Tessla.Out) :: statements =>
        result.outStreams.find(_.name == out.name).foreach {
          previous =>
            warn(ConflictingOut(out.loc, previous = previous.loc))
        }
        val newOut = ScopedTessla.OutStream(translateExpression(out.expr), out.idOpt, out.loc)
        translateStatements(statements, result.copy(outStreams = result.outStreams :+ newOut))
    }
  }

  def translateExpression(expr: Tessla.Expression): ScopedTessla.Expression = ???
}
