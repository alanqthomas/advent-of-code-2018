package com.alanqthomas

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

  val pointsWithIndex = points.zipWithIndex.map{ case (p: Point, idx: Int) => (idx, p) }.toMap
  val grid = (for {
    xs <- xMin to xMax
    ys <- yMin to yMax
  } yield Point(xs, ys)).toList

  if (true) {
    println(s"grid points: ${grid.length}")
//    println(grid)
  }

  val closestPoints = grid.map { gp => pointsWithIndex.minBy{ case (_: Int, p: Point) => Point.manhattanDistance(gp, p)} }

  if (true) {
    println(s"closestPointsSize = ${closestPoints.length}")
    println(s"closestPoints = ${closestPoints}")
  }

  val groupedPoints = closestPoints.groupBy(_._1).mapValues(_.length).toList.sortBy(_._2)
  val mostNeighbors = groupedPoints.maxBy(_._2)
  val pointWithMostNeighbors = pointsWithIndex(mostNeighbors._1)

  if (true) {
    println(s"groupedPoints.length ${groupedPoints.size}")
    println(s"$groupedPoints")
    println(s"pointWithMostNeighbors = ${mostNeighbors}")
    println(s"pointWithMostNeighbors = ${pointWithMostNeighbors}")
  }


}
