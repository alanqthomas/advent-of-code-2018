package com.alanqthomas

import scala.io.Source
import java.io.File

object Common {
  val rootPath: String = new File("C:\\Users\\Alan\\Documents\\Projects\\advent-of-code-2018\\input").getAbsolutePath

  def getLinesFromFile(filename: String): List[String] = {
    val file = new File(s"$rootPath\\$filename")

    val it = for (
      line <- Source.fromFile(file).getLines
    ) yield line

    it.toList
  }



}
