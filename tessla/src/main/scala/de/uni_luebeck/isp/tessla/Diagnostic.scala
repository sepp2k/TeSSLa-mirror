package de.uni_luebeck.isp.tessla

abstract class Diagnostic extends Exception {

}

abstract class Fatal extends Diagnostic

case class GiveUp() extends Fatal