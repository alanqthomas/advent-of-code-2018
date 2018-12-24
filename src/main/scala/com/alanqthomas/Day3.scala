package com.alanqthomas

object Day3 extends App {
  val file = Common.getLinesFromFile("day3-input.txt")

  // Part 1
  case class Point(x: Int, y: Int)
  case class Rectangle(id: Int, origin: Point, width: Int, height: Int) {
    val corner: Point = Point(origin.x + width - 1, origin.y + height - 1)
  }

  // Parse to objects
  val pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r
  val claims = for (line <- file) yield {
    val pattern(id, x, y, width, height) = line
    Rectangle(id.toInt, Point(x.toInt, y.toInt), width.toInt, height.toInt)
  }

  // Map of (claimId -> Set of all points in claim)
  val pointsByClaim = claims.map(claim => {
    val points = (for {
      x <- claim.origin.x to claim.corner.x
      y <- claim.origin.y to claim.corner.y
    } yield Point(x, y)).toSet

    claim.id -> points
  }).toMap

  val overlappedPoints: Set[Point] = pointsByClaim.values.foldLeft( (Set.empty[Point], Set.empty[Point]) ) {
    case ((all: Set[Point], dupes: Set[Point]), newPoints: Set[Point]) =>
      val commonPoints = newPoints & all

      val newDupes = dupes ++ commonPoints
      val newAll = all ++ newPoints

      (newAll, newDupes)
  }._2

  // Answer
  println("Square inches of overlapping space")
  println(overlappedPoints.size)
  println()

  // Part 2
  val nonOverlappingClaim = pointsByClaim.filter {
    case (claimId, pointsInSet) => (pointsInSet & overlappedPoints).isEmpty
  }

  // Answer
  println("Non overlapping claim")
  println(nonOverlappingClaim.head._1)
}
