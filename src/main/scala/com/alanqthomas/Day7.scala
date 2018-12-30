package com.alanqthomas

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

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

  // Part 1
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

  // Answer
  val sortedGraph = sortGraph(graph)
  println(s"sortedGraph = ${sortedGraph.mkString("")}")

  // Part 2
  def getPossibleWork(g: Graph[String, DiEdge]): Option[List[String]] =
    getHeadLayer(g).map(_._2.toList.map(_.value))

  case class Work(step: String, timeRemaining: Int)

  val numberOfWorkers = 5
  def workTimeForStep(step: String): Int = 60 + step.toCharArray.head - 64

  val allCompleted: ListBuffer[String] = ListBuffer()

  def makeNewWorkers(workers: Map[Int, Option[Work]], eligibleWork: List[String], eligibleWorkers: List[Int]): Map[Int, Option[Work]] = {
    if (eligibleWork.isEmpty || eligibleWorkers.isEmpty) workers
    else {
      val sortedWork = eligibleWork.sorted
      val worker = eligibleWorkers.head
      val work = sortedWork.head

      val newWorkers: Map[Int, Option[Work]] = workers + (worker -> Some(Work(work, workTimeForStep(work))))

      makeNewWorkers(newWorkers, sortedWork.tail, eligibleWorkers.tail)
    }
  }

  def totalWorkingTime(graph: Graph[String, DiEdge]): Int = {
    @tailrec
    def rec(g: Graph[String, DiEdge], workers: Map[Int, Option[Work]], currentTime: Int): Int = {
      if (g.isEmpty && workers.values.forall(_.forall(_.timeRemaining == 0))) currentTime - 1
      else {
        var stepsCompleted: ListBuffer[String] = ListBuffer()

        val updatedWorkers: Map[Int, Option[Work]] = workers.map {
          case (id, Some(work)) =>
            val newTimeRemaining = work.timeRemaining - 1

            if (newTimeRemaining == 0) {
              stepsCompleted += work.step
              allCompleted += work.step
              id -> None
            } else id -> Some(work.copy(timeRemaining = newTimeRemaining))
          case worker => worker
        }

        val newGraph = g -- stepsCompleted

        val possibleWork = getPossibleWork(newGraph).map(_.toSet).getOrElse(Set.empty)
        val currentlyWorking = updatedWorkers.values.flatMap(_.map(_.step)).toSet
        val eligibleWork = possibleWork -- currentlyWorking

        val eligibleWorkers = updatedWorkers.filter{ case (_, maybeWork) => maybeWork.isEmpty }.keys
        val newWorkers = makeNewWorkers(Map.empty[Int, Option[Work]], eligibleWork.toList, eligibleWorkers.toList)
        val idleWorkers = (eligibleWorkers.toSet -- newWorkers.keySet).map(id => id -> Option.empty[Work])
        val allNewWorkers = updatedWorkers ++ newWorkers ++ idleWorkers
        def workerStatus(id: Int): String = allNewWorkers(id).map(w => s"${w.timeRemaining} - ${w.step}").getOrElse(".")

        if (false) {
          println(String.format(
            "|%6s|%8s|%8s|%8s|%8s|%8s|%-26s|",
            currentTime.toString,
            workerStatus(0),
            workerStatus(1),
            workerStatus(2),
            workerStatus(3),
            workerStatus(4),
            allCompleted.mkString("")
          ))
        }

        rec(newGraph, allNewWorkers, currentTime + 1)
      }
    }

    println("--------------------------------------------------------------------------------")
    println("|Second|Worker 1|Worker 2|Worker 3|Worker 4|Worker 5|Done                      |")
    println("--------------------------------------------------------------------------------")
    val workers = (0 until numberOfWorkers).map(_ -> Option.empty[Work]).toMap
    rec(graph, workers, 0)
  }

  // Answer
  val totalWorkTime = totalWorkingTime(graph)
  println("--------------------------------------------------------------------------------")
  println(s"totalWorkTime = ${totalWorkTime}")

}
