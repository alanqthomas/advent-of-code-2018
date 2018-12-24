package com.alanqthomas

object Day5 extends App {
  val file = Common.getLinesFromFile("day5-input.txt")

  val polymerString = file.head

  // Part 1
  def reactPolymer(polymer: String): String = {
    val polarityDifference = 32

    def rec(prev: String, rest: String): String = {
      if (rest.length < 2) prev + rest
      else {
        val first = rest.head
        val second = rest(1)

        if (Math.abs(second - first) == polarityDifference) {
          val last = if (prev.isEmpty) "" else prev.last
          rec(prev.dropRight(1), last + rest.substring(2))
        } else {
          rec(prev + rest.take(1), rest.substring(1))
        }
      }
    }

    rec("", polymer)
  }

  // Part 2
  def removeChar(polymer: String, removeChar: Char): String = {
    val substitutions = Map(
      removeChar -> "",
      removeChar + 32 -> ""
    )

    polymer.map(c => substitutions.getOrElse(c, c).toString).mkString("")
  }

  val shortenedPolymers = ('A' to 'Z').map(c => c -> removeChar(polymerString, c)).toMap
  val reactedPolymers = shortenedPolymers.mapValues(poly => reactPolymer(poly).length).toList.sortBy(p => p._2)
  println(s"reactedPolymers = ${reactedPolymers}")
}
