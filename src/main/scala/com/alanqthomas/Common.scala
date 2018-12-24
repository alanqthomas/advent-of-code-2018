package com.alanqthomas

import scala.io.Source
import java.io.File

object Common {
  val windowsRootPath: String = new File("C:\\Users\\Alan\\Documents\\Projects\\advent-of-code-2018\\input").getAbsolutePath
  val macRootPath: String = new File("/Users/alanqthomas/Documents/projects/advent-of-code-2018/input").getAbsolutePath

  val isMac: Boolean = true

  def getLinesFromFile(filename: String): List[String] = {
    val file = if (isMac) new File(s"$macRootPath/$filename")
    else new File(s"$windowsRootPath\\$filename")

    val it = for (
      line <- Source.fromFile(file).getLines
    ) yield line

    it.toList
  }



}
