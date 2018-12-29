package com.alanqthomas

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import scala.annotation.tailrec

object Day7 extends App {
  val file = Common.getLinesFromFile("day7-input.txt")

  val rulePattern = "Step (.) must be finished before step (.) can begin.".r
  val edges: List[DiEdge[String]] = file.map(line => {
    line.trim match {
      case rulePattern(before, after) => before ~> after
    }
  })

  val graph = Graph(edges: _*)

  def getHeadLayer(g: Graph[String, DiEdge]): Option[(Int, Iterable[g.NodeT])] = {
    g.topologicalSort.fold(
      cycleNode => {
        println(s"[Error] Cycle at node: $cycleNode")
        None
      },
      sort => Some(sort.toLayered.head)
    )
  }

  @tailrec
  def sortGraph(g: Graph[String, DiEdge], sorted: List[String] = List.empty): List[String] = {
    if (g.isEmpty) sorted
    else {
      getHeadLayer(g) match {
        case None => sorted
        case Some(headLayer) =>
          val elem = headLayer._2.toList.map(_.value).min
          sortGraph(g - elem, sorted :+ elem)
      }
    }
  }

  val sortedGraph = sortGraph(graph)
  println(s"sortedGraph = ${sortedGraph.mkString("")}")
}
