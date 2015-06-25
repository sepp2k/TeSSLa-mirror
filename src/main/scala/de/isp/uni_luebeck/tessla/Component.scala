package de.isp.uni_luebeck.tessla

/**
 * @author nd
 */
abstract class Component{//(val inputs: Seq[InputNode], val outputs: Seq[OutputNode]) {
  def implements(op: Operation): Boolean
  
  def inputArity: Int
  def outputArity: Int
   
}

