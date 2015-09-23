package de.uni_luebeck.isp.tessla.modules2

import org.json4s._
import org.json4s.JsonDSL._

/**
 * @author Normann Decker <decker@isp.uni-luebeck.de>
 */

object Module {

  /**
   * Creates the JSON AST for a list of Modules
   */
  def json(modules: List[Module]): JObject = {
    val id = modules.zip(Stream from 1).toMap
    ("@type" -> "java.util.Collections$UnmodifiableSet") ~
      ("@items" -> modules.map { m => m.toJSON(id) })
  }
}

abstract class Module {
  val typeString: String
  val outputWidth: Int

  def toJSON(id: Map[Module, Int]): JObject = {
    (("@id" -> id(this)) ~
      ("@type" -> typeString)) ~
      ("outputWidth" -> outputWidth) ~
      specificMembers(id)
  }

  def specificMembers(id: Map[Module, Int]): JObject = JObject()
}

case class ApplicationMessageID() extends Module {
  val typeString = "dataFlowGraph.node.input.applicationMessage.ApplicationMessageID"
  val outputWidth = 32
}

case class ConstantNode(val value: Int) extends Module {
  val typeString = "dataFlowGraph.node.input.ConstantNode"
  val outputWidth = 32

  override def specificMembers(id: Map[Module, Int]): JObject = { ("value" -> value) }
}

case class EqualNode(var operandA: Module, var operandB: Module) extends Module {
  val typeString = "dataFlowGraph.node.operation.EqualNode"
  val outputWidth = -1

  override def specificMembers(id: Map[Module, Int]): JObject = {
    ("operandA" -> id(operandA)) ~ ("operandB" -> id(operandB))
  }
}

case class ItThenNode(var control: Module, var trueNode: Module) extends Module {
  val typeString = "dataFlowGraph.node.operation.IfThenNode"
  val outputWidth = -1

  override def specificMembers(id: Map[Module, Int]): JObject = {
    ("control" -> id(control)) ~ ("trueNode" -> id(trueNode))
  }
}

case class ApplicationMessageValue() extends Module {
  val typeString = "dataFlowGraph.node.input.applicationMessage.ApplicationMessageValue"
  val outputWidth = 32
}

case class RegChangeMessageID() extends Module {
  val typeString = "dataFlowGraph.node.input.regChangeMessage.RegChangeMessageID"
  val outputWidth = 32
}

case class RegChangeMessageValue() extends Module {
  val typeString = "dataFlowGraph.node.input.ConstantNode"
  val outputWidth = 32
}

case class AndNode(var operandA: Module, var operandB: Module) extends Module {
  val typeString = "dataFlowGraph.node.operation.AndNode"
  val outputWidth = -1

  override def specificMembers(id: Map[Module, Int]): JObject = {
    ("operandA" -> id(operandA)) ~ ("operandB" -> id(operandB))
  }
}

case class MessageSrcNode() extends Module {
  val typeString = "dataFlowGraph.node.input.ConstantNode"
  val outputWidth = 8
}

case class OutputNode(var inputNode: Module) extends Module {
  val typeString = "dataFlowGraph.node.OutputNode"
  val outputWidth = -1

  override def specificMembers(id: Map[Module, Int]): JObject = {
    ("inputNode" -> id(inputNode))
  }
}

  


