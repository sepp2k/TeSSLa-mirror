package de.uni_luebeck.isp.tessla

import org.json4s._
import org.json4s.native.JsonMethods._

case class ModuleGraph(json: JObject) extends WithDebugOutput{
  override def debugOutput = pretty(render(json))

}
