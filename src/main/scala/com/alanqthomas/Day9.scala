package com.alanqthomas

import scala.collection.mutable

object Day9 extends App {
  val numPlayers = 428
  val highestValue = 70825

  // Test input 1
//  val numPlayers = 9
//  val highestValue = 25

  // Test input 2
//  val numPlayers = 17
//  val highestValue = 50
//  val highestValue = 1104


  val x = mutable.Queue

  case class CircleNode[T](value: T, var next: CircleNode[T], var prev: CircleNode[T])

  class Circle[T](var head: CircleNode[T], var tail: CircleNode[T]) {
    def append(value: T): Unit = {
      val node = CircleNode(value, head, tail)
      head.prev = node
      tail.next = node
      tail = node
    }

    def pop(): T = {
      val value = head.value
      val nextNode = head.next
      nextNode.prev = tail
      tail.next = nextNode
      head = nextNode

      value
    }

    def rotate(n: Int): Unit = {
      def move(remaining: Int, node: CircleNode[T], moveF: CircleNode[T] => CircleNode[T]): CircleNode[T] = {
        if (remaining == 0) node
        else {
          val newNode = moveF(node)
          move(remaining - 1, newNode, moveF)
        }
      }

      val (remaining, moveF) = if (n >= 0) (n + 1, (c: CircleNode[T]) => c.prev)
        else (Math.abs(n), (c: CircleNode[T]) => c.next)

      val newHead = move(remaining, head, moveF)
      head = newHead
      tail = newHead.prev
    }

    override def toString: String = {
      def rec(node: CircleNode[T]): String = {
        if (node == tail) s"${node.value}"
        else {
          s"${node.value} -> ${rec(node.next)}"
        }
      }

      rec(head)
    }
  }

  object Circle {
    def apply[T](value: T): Circle[T] = {
      val node = CircleNode[T](value, null, null)
      node.next = node
      node.prev = node

      new Circle(node, node)
    }
  }

  def play(numPlayers: Int, highestValue: Long): Long = {
    val circle = Circle(0L)
    val scores: mutable.Map[Int, Long] = mutable.Map()

    (1L until highestValue).foreach(marble => {
//      println(s"$marble CURRENT MARBLE")
//      println("CIRCLE BEFORE")
//      println(circle)
//      println("SCORES")
//      println(scores)

      if (marble % 23 == 0) {

        circle.rotate(7)
        val player = (marble % numPlayers).toInt
        val popped = circle.pop()
        val points = marble + popped
//        println("SCORING")
//        println("player")
//        println(player)
//        println("popped")
//        println(popped)
//        println("points")
//        println(points)
        scores += (player -> (scores.getOrElse(player, 0L) + points))
      } else {
//        println("ELSE")
        circle.rotate(-1)
        circle.append(marble)
      }

//      println("CIRCLE AFTER")
//      println(circle)
//      println("=====")
    })

    scores.values.max
  }

  val highScore = play(numPlayers, highestValue)
  println("RESULT")
  println(highScore)

  def getCurrentPlayer(currentValue: BigInt, numPlayers: BigInt): BigInt = (currentValue - 1) % numPlayers
  def getInsertLocation(currentLocation: BigInt, circleSize: BigInt): BigInt = (currentLocation + 2) % circleSize
  def getScoringLocation(currentLocation: BigInt, circleSize: BigInt): BigInt = {
    val newLocation = currentLocation - 7
    if (newLocation >= 0) newLocation
    else circleSize + newLocation
  }

//  def playGame(numPlayers: BigInt, highestValue: BigInt): Map[BigInt, BigInt] = {
//    def rec(currentValue: BigInt, currentLocation: BigInt, circle: mutable.Queue[BigInt], scores: Map[BigInt, BigInt]): Map[BigInt, BigInt] = {
//
//      val currentPlayer = getCurrentPlayer(currentValue, numPlayers)
//
//      if (currentValue > highestValue) {
//        println("BASE CASE")
//        scores
//      }
//      else if (currentValue % 23 == 0) {
//        val scoringLocation = getScoringLocation(currentLocation, circle.length)
//        val points = currentValue + circle(scoringLocation.toInt)
//        val newScore = scores.getOrElse(currentPlayer, BigInt(0)) + points
//        val newScores = scores + (currentPlayer -> newScore)
//
////        println("\n=====")
////        println("REC")
////        println(s"currentValue = ${currentValue}")
////        println(s"value at scoring location = ${circle(scoringLocation)}")
////        println(s"currentLocation = ${currentLocation}")
////        println(s"circle = ${circle}")
////        println(s"scores = ${scores}")
////        println()
////        println(s"currentPlayer = ${currentPlayer}")
////        println(s"scoringLocation = ${scoringLocation}")
////        println(s"points = ${points}")
////        println(s"newScore = ${newScore}")
////        println(s"newScores = ${newScores}")
//
//        val (first, rest) = circle.splitAt(scoringLocation.toInt)
//        val newCircle = first ++ rest.tail
//        rec(currentValue + 1, scoringLocation, newCircle, newScores)
//      } else {
//        val insertLocation = getInsertLocation(currentLocation, circle.length)
//        val (first, rest) = circle.splitAt(insertLocation.toInt)
//        val newCircle = (first :+ currentValue) ++ rest
//
////        println("\n=====")
////        println("REC")
////        println(s"currentValue = ${currentValue}")
////        println(s"currentLocation = ${currentLocation}")
////        println(s"circle = ${circle}")
////        println(s"scores = ${scores}")
////        println(s"currentPlayer = ${currentPlayer}")
////        println(s"insertLocation = ${insertLocation}")
////        println(s"first = ${first}")
////        println(s"rest = ${rest}")
////        println(s"newCircle = ${newCircle}")
//        rec(currentValue + 1, insertLocation, newCircle, scores)
//      }
//    }
//
////    rec(0, 0, mutable.Queue.empty, Map.empty)
//    rec(1, 0, mutable.Queue(0), scores)
//  }

//  val tests = List(
//    (0, 1),
//    (1, 2),
//    (1, 3),
//    (1, 5),
//    (7, 8)
//  ).map(Function.tupled(getInsertLocation))
//  tests.foreach(println)

//  println()
//  println("RESULTS")
//  println("=====")
//  val scores = playGame(numPlayers, highestValue)
//  println(s"scores = ${scores}")
//
//  val highScore = scores.maxBy(_._2)
//  println(s"highScore = ${highScore}")

}
