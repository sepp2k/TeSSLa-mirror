package de.uni_luebeck.isp.tessla.modules

import org.json4s._
import org.json4s.JsonDSL._
import scala.collection.mutable

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

  //  def dereference[K](mapping: mutable.Map[K, Module]): Module

  def map(f: Module => Module): Module = this

}

case class GenericModule(val name: String = "", var inputs: List[Module] = List()) extends Module {
  val typeString = "GenericModule"
  val outputWidth = -1

  override def specificMembers(id: Map[Module, Int]): JObject = {
    ("inputs" -> inputs.map(id)) ~ ("name" -> name)
  }

  override def map(f: Module => Module): GenericModule = {
    inputs = inputs.map(f)
    this
  }
}

case class ConstantNode(val value: Int) extends Module {
  val typeString = "dataFlowGraph.node.input.ConstantNode"
  val outputWidth = 32

  override def specificMembers(id: Map[Module, Int]): JObject = { ("value" -> value) }
}

case class ApplicationMessageID() extends Module {
  val typeString = "dataFlowGraph.node.input.applicationMessage.ApplicationMessageID"
  val outputWidth = 32
}

case class EqualNode(var operandA: Module, var operandB: Module) extends Module {
  val typeString = "dataFlowGraph.node.operation.EqualNode"
  val outputWidth = -1

  override def specificMembers(id: Map[Module, Int]): JObject = {
    ("operandA" -> id(operandA)) ~ ("operandB" -> id(operandB))
  }
  override def map(f: Module => Module) = {
    operandA = f(operandA)
    operandB = f(operandB)
    this
  }
}

case class ItThenNode(var control: Module, var trueNode: Module) extends Module {
  val typeString = "dataFlowGraph.node.operation.IfThenNode"
  val outputWidth = -1

  override def specificMembers(id: Map[Module, Int]): JObject = {
    ("control" -> id(control)) ~ ("trueNode" -> id(trueNode))
  }

  override def map(f: Module => Module) = {
    control = f(control)
    trueNode = f(trueNode)
    this
  }
}

case class IfThenElseNode(var control: Module, var trueNode: Module, var falseNode: Module) extends Module {
  val typeString = "dataFlowGraph.node.operation.IfThenNode"
  val outputWidth = -1

  override def specificMembers(id: Map[Module, Int]): JObject = {
    ("control" -> id(control)) ~ ("trueNode" -> id(trueNode)) ~ ("falseNode" -> id(falseNode))
  }

  override def map(f: Module => Module): IfThenElseNode = {
    control = f(control)
    trueNode = f(trueNode)
    falseNode = f(falseNode)
    this
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

  override def map(f: Module => Module) = {
    inputNode = f(inputNode)
    this
  }
}