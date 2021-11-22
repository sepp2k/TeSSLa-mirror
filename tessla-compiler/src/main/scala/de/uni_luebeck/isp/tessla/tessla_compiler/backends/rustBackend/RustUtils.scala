package de.uni_luebeck.isp.tessla.tessla_compiler.backends.rustBackend

class RustUtils {}

case class SourceSegments(
  variables: StringBuilder = new StringBuilder,
  input: StringBuilder = new StringBuilder,
  timestamp: StringBuilder = new StringBuilder,
  computation: StringBuilder = new StringBuilder,
  output: StringBuilder = new StringBuilder,
  static: StringBuilder = new StringBuilder,
  store: StringBuilder = new StringBuilder
)
