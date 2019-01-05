package com.alanqthomas

object Day11 extends App {
  val gridSerialNumber = 8141

  case class Point(x: Int, y: Int)
  case class Square(point: Point, size: Int, power: Int)

  def calculatePowerLevel(point: Point, gsn: Option[Int] = None): Int = {
    val rackId = point.x + 10
    val a = rackId * point.y
    val b = a + gsn.getOrElse(gridSerialNumber)
    val c = b * rackId
    val d = (c / 100).toString.last.toString.toInt
    val e = d - 5
    e
  }

  def printGrid(grid: List[(Int, List[Int])], asCoordinates: Boolean = false): Unit = {
    print("    :")
    (1 to 300).foreach(x => print(String.format("%4s", x.toString)))
    println()
    grid.foreach {
      case (y, xs) =>
        print(String.format("%4s:", y.toString))
        xs.foreach(x => print(String.format("%4s", x.toString)))
        println()
    }
  }

  val grid = (1 to 300).toList.map(y => {
    y -> (1 to 300).toList
  })

  val powerLevelGrid = grid.map {
    case (y, xs) =>
      y -> xs.map(x => {
        calculatePowerLevel(Point(x, y))
      })
  }

  val squarePowerLevels = (0 to 297).toList.map(y => {
    y -> (0 to 297).toList
  }).map {
    case (y, xs) =>
      y -> xs.map(x => (y until y + 3).flatMap(yy => powerLevelGrid(yy)._2.slice(x, x + 3)).sum)
  }

  val sorted = {
    val s = squarePowerLevels.map(spl => spl._1 -> spl._2.sortWith(_ > _))
    s.sortBy(_._2.head)
  }

  // Part 1
  printGrid(sorted)

  val powerLevelMatrix = Array.ofDim[Int](300, 300)

  for {
    x <- 0 until 300
    y <- 0 until 300
  } {
    powerLevelMatrix(y)(x) = calculatePowerLevel(Point(x + 1, y + 1))
  }
  val sizeToPowerLevelMatrix = Array.ofDim[Int](300, 300, 300)

  for {
    x    <- 0 until 300
    y    <- 0 until 300
    size <- 1 to (300 - Math.max(x, y))
  } {
    val xEnd = x + size - 1
    val yEnd = y + size - 1
    val power = (x to xEnd).map(xx => powerLevelMatrix(yEnd)(xx)).sum +
      (y until yEnd).map(yy => powerLevelMatrix(yy)(xEnd)).sum
    val prevPower = sizeToPowerLevelMatrix(y)(x)(Math.max(size - 2, 0))
    sizeToPowerLevelMatrix(y)(x)(size - 1) = power + prevPower
  }

  val squares = for {
    x    <- 0 until 300
    y    <- 0 until 300
    size <- 1 to (300 - Math.max(x, y))
  } yield {
    Square(Point(x, y), size, sizeToPowerLevelMatrix(y)(x)(size - 1))
  }

  val maxSquare = squares.maxBy(_.power)
  println(s"max square = $maxSquare")
}
