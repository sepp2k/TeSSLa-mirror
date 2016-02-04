package de.uni_luebeck.isp.tessla

class Compiler {
  var diagnostics: Seq[Diagnostic] = Seq()

  def diagnostic(diagnostic: Diagnostic) { diagnostics :+= diagnostic }
}
