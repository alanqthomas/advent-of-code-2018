package com.alanqthomas

import scala.io.Source
import java.io.File

object Common {
  def getLinesFromFile(filename: String): List[String] = {
    val file = new File(s"input/$filename")

    val it = for (
      line <- Source.fromFile(file).getLines
    ) yield line

    it.toList
  }

  sealed trait Direction
  case object UP extends Direction
  case object DOWN extends Direction
  case object LEFT extends Direction
  case object RIGHT extends Direction

  case class Point(x: Int, y: Int) extends Ordered[Point] {
    def move(direction: Direction): Point = {
      direction match {
        case UP       => Point(x,     y - 1 )
        case DOWN     => Point(x,     y + 1 )
        case LEFT     => Point(x - 1, y     )
        case RIGHT    => Point(x + 1, y     )
      }
    }

    override def compare(that: Point): Int = {
      if (y == that.y && x == that.x) 0
      else if (y == that.y) x.compareTo(that.x)
      else y.compareTo(that.y)
    }

    override def toString: String = s"($x, $y)"
  }

}
