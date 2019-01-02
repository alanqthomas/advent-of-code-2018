package com.alanqthomas

object Day10 extends App {
  val file = Common.getLinesFromFile("day10-input.txt")

  case class Point(x: Int, y: Int)
  case class Velocity(x: Int, y: Int)

  val pattern = "position=<\\s*(-?\\d+),\\s*(\\-?\\d+)> velocity=<\\s*(\\-?\\d+),\\s*(\\-?\\d+)>".r

  val (positions, velocities) = file.map(line => {
    line.trim match {
      case pattern(x, y, velX, velY) =>
        (Point(x.toInt, y.toInt), Velocity(velX.toInt, velY.toInt))
    }
  }).unzip

  def getRanges(positions: List[Point]) = {
    val xMax = positions.maxBy(_.x).x
    val xMin = positions.minBy(_.x).x
    val yMax = positions.maxBy(_.y).y
    val yMin = positions.minBy(_.y).y
    (Range(xMin, xMax), Range(yMin, yMax))
  }

  val (xRange, yRange) = getRanges(positions)

  if (true) {
    println(s"xRange = ${xRange.length}")
    println(s"yRange = ${yRange.length}")
    println("Positions")
    println(positions.length)
    positions.foreach(println)
    println()
    println("Velocities")
    positions.foreach(println)
  }

  def printGridIfSmall(positions: List[Point]): Unit = {
  val (xRange, yRange) = getRanges(positions)

    if (yRange.end - yRange.start < positions.length) {
      val grid = (yRange.start to yRange.end).toList.map(y => {
        y -> (xRange.start to xRange.end).toList
      })
      grid.foreach {
        case (y, xs) =>
          xs.foreach(x => {
            val p = Point(x, y)
            val char = positions.find(_ == p).map(_ => "#").getOrElse(".")
            print(char)
          })
          println()
      }
      println("\n=========================\n")
    } else println("SKIPPING GRID\n")
  }

  def alignStars(
    initialPositions: List[Point],
    velocities: List[Velocity],

    totalSeconds: Int,
    startSecond: Int = 0): Unit = {
    def rec(positions: List[Point], currentSecond: Int): Unit = {
      println("REC")
      if (currentSecond == totalSeconds) ()
      else {
        println(s"SECOND: $currentSecond")
        printGridIfSmall(positions)
        val newPositions = (positions zip velocities).map {
          case (position, velocity) => Point(position.x + velocity.x, position.y + velocity.y)
        }

        rec(newPositions, currentSecond + 1)
      }
    }

    rec(initialPositions,  startSecond)
  }

  val maxSeconds = 11000
  alignStars(positions, velocities, maxSeconds)

}
