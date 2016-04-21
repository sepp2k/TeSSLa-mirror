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
          val moore = RltlConv.convert("assert " + node.args.head.node.function.asInstanceOf[ConstantValue[_]].value.toString,"--salt","--ltl","--moore","--min").asInstanceOf[Moore].toNamedMoore
          val stateMap = moore.states.foldLeft(Map[String,String]())((m, s) => m + (s.name -> moore.labels(s).toString))
          val allTrans = moore.transitions.filter(element => !moore.alphabet.contains(element._1._2.toString)).map(element => (element._1._1,element._2))
          val trans = (moore.alphabet.foldLeft(List[(String,String,String)]())((l,a) => l ++ allTrans.map(s => (s._1.name,a,s._2.name))) ++
            moore.transitions.filter(element => moore.alphabet.contains(element._1._2.toString)).toList.map(e => (e._1._1.name,e._1._2.toString,e._2.name))).map(e => (e._1,e._2) -> e._3).toMap
          val transitionMap = moore.states.foldLeft(Map[String,Map[String,String]]())((m,s) => {
            val signToNext = trans.filter(element => element._1._1 == s.name).map(element => element._1._2.filterNot("\"".toSet)
              .replaceAll("&&",",").replaceAll("\\(","\\{").replaceAll("\\)","\\}") -> element._2)
            m + (s.name -> signToNext)
        } )
        nodes.update(id, Node(id,StateMachineFunction(f.name,f.signature,stateMap,transitionMap),node.args.tail))
        }
        case _ =>
      }
    }
    graph
  }
}