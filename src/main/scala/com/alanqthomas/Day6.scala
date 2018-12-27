package com.alanqthomas

import java.io.{BufferedWriter, File, FileWriter}

object Day6 extends App {
  val file = Common.getLinesFromFile("day6-input.txt")

  case class Point(x: Int, y: Int)
  object Point {
    def manhattanDistance(pointA: Point, pointB: Point): Int =
      Math.abs(pointA.x - pointB.x) + Math.abs(pointA.y - pointB.y)
  }

  val points = for (line <- file) yield {
    val parts = line.split(", ")
    Point(parts(0).toInt, parts(1).toInt)
  }


  if (false) {
    println("points")
    println(points.length)
    points.foreach(println)
  }

  val pointsSortedByX = points.sortBy(_.x)
  val pointsSortedByY = points.sortBy(_.y)

  if (false) {
    println(s"pointsSortedByX = ${pointsSortedByX}")
    println(s"pointsSortedByY = ${pointsSortedByY}")
  }

  val xMin = points.minBy(_.x).x
  val xMax = points.maxBy(_.x).x
  val yMin = points.minBy(_.y).y
  val yMax = points.maxBy(_.y).y

  if (true) {
    println(s"xMin = ${xMin}")
    println(s"xMax = ${xMax}")
    println(s"yMin = ${yMin}")
    println(s"yMax = ${yMax}")
  }

  val minByX = points.sortBy(_.x)
  val maxByX = points.sortWith(_.x > _.x)
  val minByY = points.sortBy(_.y)
  val maxByY = points.sortWith(_.y > _.y)

  if (false) {
    println(s"minByX = ${minByX}")
    println(s"maxByX = ${maxByX}")
    println(s"minByY = ${minByY}")
    println(s"maxByY = ${maxByY}")
  }

  def filterOutInfinitePoints(points: List[Point]): List[Point] =
    points.filterNot(p => p.x == xMin || p.x == xMax || p.y == yMin || p.y == yMax)

  val labels = (('A' to 'Z') ++ ('a' to 'z')).map(_.toString).zipWithIndex.map(zwi => zwi._2 -> zwi._1).toMap

  val pointsWithIndex = points.indices zip points
//  val pointsWithIndex = points.zipWithIndex.map{ case (p: Point, idx: Int) => (idx, p) }.toMap
  val grid = (for {
    xs <- xMin to xMax
    ys <- yMin to yMax
  } yield Point(xs, ys)).toList

//  val closestPoints = grid.map { gp => pointsWithIndex.minBy{ case (_: Int, p: Point) => Point.manhattanDistance(gp, p)}._1 -> gp }

  val closestPoints = grid.map { gp =>
    val distances = pointsWithIndex.map {
      case (id, p) => id -> Point.manhattanDistance(gp, p)
    }.sortBy(_._2)

    if (distances(0)._2 == distances(1)._2) 51 -> gp
    else distances.head._1 -> gp
  }
  val groupedPoints = closestPoints.groupBy(_._1)

  val groupedPointsFiltered = groupedPoints.filterNot {
    case (_, ps) => ps.exists { case (_, p) => p.x == xMin || p.x == xMax || p.y == yMin || p.y == yMax }
  }
  val groupedPointsFilteredSizes = groupedPointsFiltered.mapValues(_.length).toList.sortBy(_._2)

  val mostNeighbors = groupedPointsFilteredSizes.maxBy(_._2)
  val pointWithMostNeighbors = pointsWithIndex(mostNeighbors._1)

  if (true) {
    println(s"closestPointsSize = ${closestPoints.length}")
//    println(s"groupedPointsFiltered.size = ${groupedPoints.size}")
//    groupedPointsFilteredSizes.map{ case (id, size) => (labels(id), size) }.foreach(println)

//    println(s"groupedPoints.size = ${groupedPoints.size}")
//    groupedPoints.mapValues(_.length).toList.sortBy(_._2).map{ case (id, size) => (labels(id), size) }.foreach(println)

    println(s"mostNeighbors = ${mostNeighbors}")
    println(s"pointWithMostNeighbors = ${pointWithMostNeighbors}")
    println(s"labels(mostNeighbors._1) = ${labels(mostNeighbors._1)}")
  }

  val allPointDistances = grid.filter { gp =>
    val totalDistance = pointsWithIndex.map {
      case (_, p) => Point.manhattanDistance(gp, p)
    }.sum

    totalDistance < 10000
  }

  if (true) {
    println(s"allPointDistances.length = ${allPointDistances.length}")
    allPointDistances.foreach(println)
  }

  // Grid output
  if (false) {
    val windowsRootPath: String = new File("C:\\Users\\Alan\\Documents\\Projects\\advent-of-code-2018\\input").getAbsolutePath
    val outputFile: File = new File(s"$windowsRootPath\\output.txt")
    val bw = new BufferedWriter(new FileWriter(outputFile))

    val closestPointsSorted = closestPoints.groupBy(_._2.y).toList.sortBy(_._1)

    // Headers
    val row1 = new StringBuilder
    val row2 = new StringBuilder
    val row3 = new StringBuilder
    val xHeaders = xMin to xMax

    row1.append("    ")
    xHeaders.foreach(x => row1.append(x / 100))
    row2.append("    ")
    xHeaders.foreach(x => row2.append((x / 10) % 10))
    row3.append("    ")
    xHeaders.foreach(x => row3.append(x % 100 % 10))

    bw.write(row1.toString())
    bw.write("\n")
    bw.write(row2.toString())
    bw.write("\n")
    bw.write(row3.toString())
    bw.write("\n")
    bw.write("\n")
    // END Headers

    closestPointsSorted.foreach {
      case (y, ps) =>
        val sb = new StringBuilder

        sb.append(String.format("%3s ", y.toString))
        ps.foreach(p => {
          val elem = pointsWithIndex.find(pi => pi._2 == p._2).map(_ => "*").getOrElse(labels(p._1))
          sb.append(elem)
        })

        sb.append("\n")
        bw.write(sb.toString)
    }
    bw.close()
  }
}
