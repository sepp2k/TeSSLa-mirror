package de.uni_luebeck.isp.tessla

case class ErrorSeq(val errors: Seq[Exception]) extends Exception {

}
