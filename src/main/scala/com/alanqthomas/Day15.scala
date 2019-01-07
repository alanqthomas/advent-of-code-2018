package com.alanqthomas

import Common._

object Day15 extends App {
  val file = Common.getLinesFromFile("day15-test-input.txt")

  abstract class Space(location: Point, fighter: Option[Fighter])
  case class Wall(location: Point) extends Space(location, None) {
    override def toString: String = "#"
  }
  case class Floor(location: Point, fighter: Option[Fighter]) extends Space(location, fighter) {
    override def toString: String = "."
  }

  abstract class Fighter(id: Int, location: Point, hp: Int = 200, attack: Int = 3)
  case class Elf(id: Int, location: Point, hp: Int = 200, attack: Int = 3) extends Fighter(id, location, hp, attack) {
    override def toString: String = "E"
  }
  case class Gremlin(id: Int, location: Point, hp: Int = 200, attack: Int = 3) extends Fighter(id, location, hp, attack) {
    override def toString: String = "G"
  }


  type Cave = Array[Array[Space]]
}
