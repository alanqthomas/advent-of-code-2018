import com.alanqthomas.Common
import Function.tupled

val ids = Common.getLinesFromFile("day2-input.txt")

// Part 1
val charFrequencies = ids
  .map { _
      .groupBy(identity)
      .mapValues(_.length)
      .values
      .toList
  }

val doubles = charFrequencies.count(_.contains(2))
val triples = charFrequencies.count(_.contains(3))
doubles * triples

// Part 2
val idSet = ids.toSet

def getDiff(a: String, b: String): Int = {
  (a zip b).map(tupled((c, d) =>
    if (c == d) 0
    else 1
  )).sum
}

val cross = for {
  id <- ids
  id2 <- ids
} yield (id, id2)

val correctBoxes = cross.filter(tupled((a, b) => getDiff(a, b) == 1)).head

(correctBoxes._1 zip correctBoxes._2).collect{
  case (a, b) if a == b => a
}.mkString("")
