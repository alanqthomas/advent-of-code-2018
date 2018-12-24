package com.alanqthomas

import org.joda.time.{DateTime, DateTimeZone}
import org.joda.time.format.DateTimeFormat

object Day4 extends App {
  val file = Common.getLinesFromFile("day4-input.txt")

  sealed trait Activity { val timestamp: DateTime }
  case class Asleep(timestamp: DateTime) extends Activity
  case class Wakeup(timestamp: DateTime) extends Activity
  case class ShiftChange(timestamp: DateTime, guardId: Int) extends Activity

  case class MinuteInterval(start: Int, end: Int) {
    val timespan: Int = end - start
  }

  val pattern = "#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)".r

  val asleepPattern = "\\[(.*)\\] falls asleep".r
  val wakeupPattern = "\\[(.*)\\] wakes up".r
  val shiftChangePattern = "\\[(.*)\\] Guard #(\\d+) begins shift".r

  val dateTimeFormatter = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
  def parseDateTime(datetimeString: String): DateTime = dateTimeFormatter.parseDateTime(datetimeString).withZoneRetainFields(DateTimeZone.UTC)

  val activities: List[(Activity, String)] = for (line <- file) yield {
    println(line)
    line.trim match {
      case asleepPattern(timestamp) => (Asleep(parseDateTime(timestamp)), line)
      case wakeupPattern(timestamp) => (Wakeup(parseDateTime(timestamp)), line)
      case shiftChangePattern(timestamp, guardId) => (ShiftChange(parseDateTime(timestamp), guardId.toInt), line)
    }
  }

  def findSleepTimes(allActivities: List[Activity]): Map[Int, List[MinuteInterval]] = {
    def rec(activities: List[Activity], sleepTimes: Map[Int, List[MinuteInterval]], guard: Int, lastActivity: Activity): Map[Int, List[MinuteInterval]] = {
      if (activities.isEmpty) sleepTimes
      else {
        val lastMinute = lastActivity.timestamp.minuteOfHour.get
        val currentMinute = activities.head.timestamp.minuteOfHour.get
        val currentGuard = lastActivity match {
          case ShiftChange(_, guardId) => guardId
          case _ => guard
        }

        val currentActivity = activities.head

        val interval: Option[MinuteInterval] = (lastActivity, currentActivity) match {
          case (_: Asleep, _: Wakeup | _: ShiftChange) => Some(MinuteInterval(lastMinute, currentMinute))
          case _ => None
        }

        val newGuardTimes = interval.map(int => sleepTimes.getOrElse(currentGuard, List.empty[MinuteInterval]) :+ int)
        val newSleepTimes = newGuardTimes.map(nst => sleepTimes + (currentGuard -> nst)).getOrElse(sleepTimes)

        def printDebug() = {
          println("=====================")
          println(s"guard =           ${guard}")
          println(s"currentGuard =    ${currentGuard}")
          println(s"lastActivity =    ${lastActivity}")
          println(s"currentActivity = ${currentActivity}")
          println(s"sleepTimes =      ${sleepTimes}")
          println(s"newSleepTimes =   ${newSleepTimes}")
          println()
        }
//        printDebug()

        rec(activities.tail, newSleepTimes, currentGuard, currentActivity)
      }
    }

    val startActivity = allActivities.head.asInstanceOf[ShiftChange]
    rec(allActivities.tail, Map.empty[Int, List[MinuteInterval]], startActivity.guardId, startActivity)
  }

  val sortedActivities = activities
    .sortWith((a: (Activity, String), b: (Activity, String)) => a._1.timestamp.isBefore(b._1.timestamp))

  println("START")
  sortedActivities.foreach(x => println(String.format("%-50s %s", x._2, x._1)))

  val guardSleepIntervals = findSleepTimes(sortedActivities.map(_._1))

//  println(s"guardSleepIntervals = ${guardSleepIntervals}")

  val guardSleepTimes = guardSleepIntervals
    .mapValues(sleepTimes =>
      sleepTimes.foldLeft(0)(_ + _.timespan)
    )
//  println(s"guardSleepTimes = ${guardSleepTimes}")


  val guardWithMostSleep = guardSleepTimes.maxBy(_._2)._1

  println(s"guardWithMostSleep = ${guardWithMostSleep}")

  val allMinutes = guardSleepIntervals(guardWithMostSleep).flatMap(int => (int.start to int.end).toList)
  println(s"allMinutes: $allMinutes")


  val commonMinutes = allMinutes.groupBy(identity).mapValues(_.size)
  println(s"commonMinutes: $commonMinutes")

  val mostMinutes = commonMinutes.maxBy(_._2)._2
  println(s"mostMinutes = ${mostMinutes}")

  val bestMinute = commonMinutes.filter(a => a._2 == mostMinutes)
  println(s"bestMinute: ${bestMinute}")

  // Part 2
  val guardToMinutes = guardSleepIntervals
    .mapValues(minList =>
      minList
        .flatMap(int => (int.start to int.end).toList)
        .groupBy(identity)
        .mapValues(_.size)
        .maxBy(_._2)
    )
  println(s"guardToMinutes = ${guardToMinutes}")
  
  val x = guardToMinutes.toList.sortBy(_._2._2)
  println(s"x = ${x}")

//  println(s"answer: ${guardWithMostSleep * bestMinute}")

//    [1518-08-06 00:13] falls asleep
//    [1518-06-10 00:37] wakes up
//    [1518-07-08 00:20] wakes up
//    [1518-05-13 00:24] falls asleep
//    [1518-11-07 00:32] falls asleep
//    [1518-11-21 00:01] Guard #641 begins shift
}
