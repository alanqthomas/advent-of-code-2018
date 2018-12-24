import com.alanqthomas.Common

// Part 1
val numbers = Common.getLinesFromFile("day1-input.txt").map(_.toInt)

// Answer
numbers.sum

// Part 2
var numbersSeen = scala.collection.mutable.SortedSet[Int]()

def findRepeat(frequency: Int, ns: List[Int]): Int = {
  if (ns.isEmpty) findRepeat(frequency, numbers)
  else {
    numbersSeen += frequency

    val newFrequency = frequency + ns.head

    if (numbersSeen.contains(newFrequency)) newFrequency
    else {
      findRepeat(newFrequency, ns.tail)
    }

  }
}

// Answer
findRepeat(0, numbers)
