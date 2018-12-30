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

}
