package de.uni_luebeck.isp.tessla

import scala.util.Try
import de.uni_luebeck.isp.rltlconv.automata.Moore
import de.uni_luebeck.isp.rltlconv.cli.RltlConv

object SaltConverter extends CompilerPass[FunctionGraph, FunctionGraph] {
  override def apply(compiler: Compiler, graph: FunctionGraph) = Try {
    import graph.{Node,nodes}
    nodes.keySet.foreach { id =>  
      val node = nodes(id)
      node.function match {
        case (f : MonitorFunction) => {
          val moore = RltlConv.convert("assert " + node.args.head.node.function.asInstanceOf[ConstantValue[_]].value,"--salt","--ltl","--moore","--min").asInstanceOf[Moore].toNamedMoore
          val alphabet = moore.alphabet.map(s => "\"" + s + "\"")
          val stateMap = moore.states.foldLeft(Map[String,String]())((m, s) => m + (s.name -> moore.labels(s).toString))
          val allTrans = moore.transitions.filter(element => !alphabet.contains(element._1._2.toString)).map(element => (element._1._1,element._2))
          val alphIntSetMap = alphabet.foldLeft(Map[String,Set[Int]]())((m,a) => {
            val stringSet = if (a == "\"()\"") {
              Set()
              } else {
                a.substring(2, a.length - 2).split("&&").toSet
              }
            val intSet = stringSet.map(s => s.substring(1,2).toInt)
            m + (a -> intSet)
          } )
          val transitionList = (alphabet.foldLeft(List[(String,Set[Int],String)]())((l,a) => l ++ allTrans.map(s => (s._1.name,alphIntSetMap(a),s._2.name))) ++
            moore.transitions.filter(element => alphabet.contains(element._1._2.toString)).toList.map(e => (e._1._1.name,alphIntSetMap(e._1._2.toString),e._2.name))).map(e => (e._1,e._2,e._3))
          nodes.update(id, Node(id,StateMachineFunction(f.name,f.signature,moore.start.toString(),stateMap,transitionList),node.args.tail))
        }
        case _ =>
      }
    }
    graph
  }
}