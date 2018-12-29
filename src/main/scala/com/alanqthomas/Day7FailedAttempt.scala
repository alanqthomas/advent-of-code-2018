package com.alanqthomas

object Day7FailedAttempt extends App {
  val file = Common.getLinesFromFile("day7-test-input.txt")

  case class Rule(before: String, after: String)

  case class RuleGraph(step: String,children: Set[RuleGraph] = Set.empty[RuleGraph]) {
    override def toString: String = {
      val childGraphs =
        if (children.isEmpty) "$"
        else children.map(_.toString).mkString(",")
      s"($step -> {$childGraphs})"
    }

    def findAll(step: String): List[RuleGraph] = {
      if (this.step == step) List(this)
      else {
        if (children.isEmpty) Nil
        else children.toList.flatMap(_.find(step))
      }
    }

    def find(step: String): Option[RuleGraph] = {
      if (this.step == step) Some(this)
      else {
        if (children.isEmpty) None
        else children.toList.flatMap(_.find(step)).headOption
      }
    }

    def findNodeWithChild(step: String): Option[RuleGraph] = {
      if (children.isEmpty) None
      else {
        if (children.map(_.step).contains(step)) Some(this)
        else children.toList.flatMap(_.findNodeWithChild(step)).headOption
      }
    }

  }
  object RuleGraph {
    def constructGraph(groupedRules: Map[String, Set[String]], startAt: String): RuleGraph = {
      def rec(ruleGraph: RuleGraph): RuleGraph = {
        val maybeStepsAfter: Option[Set[String]] = groupedRules.get(ruleGraph.step)

        maybeStepsAfter match {
          case None => ruleGraph
          case Some(stepsAfter) =>
            val stepsAfterRuleGraphs = stepsAfter.map(step => rec(RuleGraph(step)))

            RuleGraph(ruleGraph.step, stepsAfterRuleGraphs)
        }
      }

      rec(RuleGraph(startAt))
    }

//    def constructFullGraph(stepIsBefore: Map[String, Set[String]], stepIsAfter: Map[String, Set[String]]): RuleGraph = {
//      val steps = ('A' to 'F').map(_.toString)
//
////      def insert(newStep: String, stepIsBefore: Set[String], setIsAfter: Set[String]): RuleGraph = {
////
////      }
//
//      var root = RuleGraph(steps.head)
//
//      steps.tail.foreach(step => {
//        val stepsAfter = stepIsBefore.getOrElse(step, Set.empty)
//        val stepsBefore = stepIsAfter.getOrElse(step, Set.empty)
//
//        val node = RuleGraph(step)
//        var current = root
//
//        while (current != null) {
//          if (stepsAfter.contains(root.step)) {
//            node.copy(children = Set(root))
//            root = node
//            current = null
//          } else {
//            val children = root.findNodeWithChild()
//          }
//        }
//
//      })
//
//      def rec(steps: List[String], root: RuleGraph): RuleGraph = {
//        if (steps.isEmpty) root
//        else {
//
//        }
//      }
//
//      rec(steps.tail, RuleGraph())
//    }

    def findLongestPath(root: RuleGraph): List[String] = {
      def rec(ruleGraph: RuleGraph): List[String] = {
        if (ruleGraph.children.isEmpty) List(ruleGraph.step)
        else {
          ruleGraph
            .children
            .toList
            .map(rg => ruleGraph.step +: rec(rg))
            .maxBy(_.length)
        }
      }

      rec(root)
    }

//    def findAllPaths(root: RuleGraph): List[List[String]] = {
//      def rec(ruleGraph: RuleGraph): List[List[String]] = {
//        if (ruleGraph.children.isEmpty) List(List(ruleGraph.step))
//        else {
//          ruleGraph
//            .children
//            .toList
//            .map(rg => List(ruleGraph.step) ++ rec(rg))
//        }
//      }
//
//      rec(root)
//    }

    def makeFullGraph(root: RuleGraph, longestPath: List[String]): List[String] = {
      def rec(ruleGraph: RuleGraph, path: List[String]): List[String] = {
        if (ruleGraph.children.isEmpty) Nil
        else {
          val currentChildren = ruleGraph.children.toList.map(_.step).toSet

          val nextChildGraph = ruleGraph.children.find(_.step == path.head).get

          val childList = rec(nextChildGraph, path.tail)

          val currentChildrenFiltered = (currentChildren -- childList.toSet).toList.sorted

          if (false) {
            println(s"path = ${path}")
            println(s"currentChildren = ${currentChildren}")
            println(s"childList = ${childList}")
            println(s"currentChildrenFiltered = ${currentChildrenFiltered}")
            println(s"currentChildrenFiltered ++ childList = ${currentChildrenFiltered ++ childList}")
            println()
          }

          currentChildrenFiltered ++ childList
        }
      }

      root.step +: rec(root, longestPath.tail)
    }

    def graphToSets(root: RuleGraph, longestPath: List[String]): List[Set[String]] = {
      def rec(ruleGraph: RuleGraph, path: List[String]): List[Set[String]] = {
        if (ruleGraph.children.isEmpty) Nil
        else {
          val nextChildGraph = ruleGraph.children.find(_.step == path.head).get

          ruleGraph.children.map(_.step) +: rec(nextChildGraph, path.tail)
        }
      }

      Set(root.step) +: rec(root, longestPath.tail)
    }

//    def setsToFullPath(sets: List[Set[String]])

  }

  val rulePattern = "Step (.) must be finished before step (.) can begin.".r
  val rules = file.map(line => {
    line.trim match {
      case rulePattern(before, after) => Rule(before, after)
    }
  })
  val groupedByBeforeRules = rules.groupBy(_.before).mapValues(_.map(_.after).toSet)
  val groupedByAfterRules = rules.groupBy(_.after).mapValues(_.map(_.before).toSet)
  val steps = ('A' to 'Z').map(_.toString)

  val ruleGraphs = steps.map(step => RuleGraph.constructGraph(groupedByBeforeRules, step))


  if (true) {
    println("RULES")
    rules.foreach(println)
    println()
  }

  if (false) {
    println("RULE GRAPHS")
    ruleGraphs.foreach(println)
    println()
  }

  val longestPaths = ruleGraphs.map(rg => rg -> RuleGraph.findLongestPath(rg))
  val longestPath = longestPaths.maxBy(_._2.length)
  val fullGraph = RuleGraph.makeFullGraph(longestPath._1, longestPath._2)

  val sets = RuleGraph.graphToSets(longestPath._1, longestPath._2)

  if (false) {
    println("SETS")
    sets.foreach(println)
    println()
  }

  if (true) {
    println("LONGEST PATHS")
    longestPaths.sortBy(_._2.length).foreach(println)
    println(s"longestPath = ${longestPath}")
    println()
  }

  if (true) {
    println(" _ is before []")
    groupedByBeforeRules.toList.sortBy(_._1).foreach(println)
    println()

    println(" _ is after []")
    groupedByAfterRules.toList.sortBy(_._1).foreach(println)
    println()
  }

  if (false) {
    val beforesAndAfters = rules.map(_.before) ++ rules.map(_.after)
    println(s"rules.length = ${rules.length}")
    println(s"beforesAndAfters.length = ${beforesAndAfters.length}")
    println(s"beforesAndAfters.toSet.size = ${beforesAndAfters.toSet.size}")
    println()
  }

  if (false) {
    println(s"rules.length = ${rules.length}")
    println(s"groupedRules.size = ${groupedByBeforeRules.size}")
//    println(s"groupedByAfterRules.size = ${groupedByAfterRules.size}")
    println()
  }

  if (false) {
    println("GROUPED RULES")
    groupedByBeforeRules.foreach(println)
    println()

    println("FULL GRAPH")
    println(s"fullGraph.length = ${fullGraph.length}")
    println(s"fullGraph = ${fullGraph.mkString("")}\n")
    println()
  }
}
