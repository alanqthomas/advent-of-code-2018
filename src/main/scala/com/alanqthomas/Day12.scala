package com.alanqthomas

object Day12 extends App {

  case class SpreadRule(rule: Byte) {
    private lazy val binString = String.format("%8s", Integer.toBinaryString(rule & 0xFF)).replace(' ', '0')
    private lazy val rulePlants: Vector[Boolean] = binString.slice(2, 5 + 2).map(SpreadRule.binCharToBool).toVector
    private lazy val result: Boolean = SpreadRule.binCharToBool(binString.last)

    override def toString: String = {
      val plants = rulePlants.map(SpreadRule.boolToPlant).mkString("")
      val n = SpreadRule.boolToPlant(result)

      s"$plants => $n"
    }

    def generationResult(plants: Boolean*): Option[Boolean] =
      if(rulePlants == plants) {
//        println(s"${this.toString} | ${plants.map(SpreadRule.boolToPlant).mkString("")}")
        Some(result)
      }
      else None

  }

  object SpreadRule {
    def apply(ll: Boolean, l: Boolean, c: Boolean, r: Boolean, rr: Boolean, n: Boolean): SpreadRule = {
      val llb = if (ll) 0x20 else 0x0
      val lb = if (l) 0x10 else 0x0
      val cb = if (c) 0x08 else 0x0
      val rb = if (r) 0x04 else 0x0
      val rrb = if (rr) 0x02 else 0x0
      val nb = if (n) 0x01 else 0x0

      val byte = (llb | lb | cb | rb | rrb | nb).toByte
      new SpreadRule(byte)
    }

    def binCharToBool(c: Char): Boolean = c match {
      case '0' => false
      case '1' => true
    }


    def binCharToPlant(c: Char): String = c match {
      case '0' => "."
      case '1' => "#"
    }

    def plantToBool(s: String): Boolean = s match {
      case "." => false
      case "#" => true
    }

    def boolToPlant(b: Boolean): String = if (b) "#" else "."

    def sumPotsWithPlants(plants: Seq[Boolean], zeroIndex: Int): Int = {
      (0 - zeroIndex until plants.length - zeroIndex)
        .zip(plants)
        .collect {
          case (idx, true) => idx
        }
        .sum
    }
  }

  val file = Common.getLinesFromFile("day12-input.txt")
  val initialStateString = file.head

  val spreadRulePattern = "(.)(.)(.)(.)(.) => (.)".r
  val spreadRules = file.drop(2).map(line => {
    line.trim match {
      case spreadRulePattern(ll, l, c, r, rr, n) =>
        SpreadRule(
          SpreadRule.plantToBool(ll),
          SpreadRule.plantToBool(l),
          SpreadRule.plantToBool(c),
          SpreadRule.plantToBool(r),
          SpreadRule.plantToBool(rr),
          SpreadRule.plantToBool(n)
        )
    }
  })

  val initialState: Vector[Boolean] = initialStateString
    .drop(15)
    .map(_.toString)
    .foldLeft(Vector.empty[Boolean])((acc, s) => {
      acc :+ SpreadRule.plantToBool(s)
    })

  def plantString(ps: Vector[Boolean]): String = ps.map(SpreadRule.boolToPlant).mkString("")

  def growPlants(initialState: Vector[Boolean], spreadRules: List[SpreadRule], maxGenerations: Long): Seq[(Int, Boolean)] = {
    def rec(plants: Vector[Boolean], generation: Long, zeroIndex: Int): (Vector[Boolean], Int) = {
      if (false) {
        println(s"GENERATION $generation ==============================================")
        println("MATCHED RULES")
        println("...RULE... | PLANTS")
      }
      val t0 = System.nanoTime()

      if (generation == maxGenerations) (plants, zeroIndex)
      else {
        val paddedPlants = Vector.fill(4)(false) ++ plants ++ Vector.fill(4)(false)
        val newState = paddedPlants
          .sliding(5)
          .toVector
          .map(ps =>
            spreadRules
              .flatMap(sr => sr.generationResult(ps: _*))
              .headOption
              .getOrElse(ps(2))
          )

        val (newPlants, newZeroIndex) = (newState, zeroIndex + 2)
//        val (newPlants, newZeroIndex) =
//          if (newState(0) || newState(1)) (newState, zeroIndex + 2)
//          else (newState.drop(2).dropRight(2), zeroIndex)

        if (false) {
          println()
          println(s"zeroIndex     = $zeroIndex")
          println(s"newState      = ${plantString(newState)}")
          println(s"paddedPlants  = ${plantString(paddedPlants)}")
          println(s"prev plants   = ${"*" * newZeroIndex}**${plantString(plants)}**${"*" * newZeroIndex}")
          println(s"newPlants     = ${"*" * zeroIndex}**${plantString(newPlants)}**${"*" * zeroIndex}")
          println(s"newZeroIndex  = $newZeroIndex")
          println("==================================================\n")
        }

        val t1 = System.nanoTime()
        val generationString = s"[${String.format("%11s", (generation + 1).toString)}]"
        println(s"$generationString: ${SpreadRule.sumPotsWithPlants(newPlants, newZeroIndex)}")
//        if (generation % 1000 == 0) println(s"[${String.format("%11s", generation.toString)}]Run time = ${(t1 - t0) / 1000000}ms")
        rec(newPlants, generation + 1, newZeroIndex)
      }
    }

    val (newPlants, zeroIndex) = rec(initialState, 0, 0)

    (0 - zeroIndex until newPlants.length - zeroIndex) zip newPlants
  }

  // Part 1
//  val numGenerations = 20
  // Part 2
  val numGenerations = 50000000000L

  val grownPlants = growPlants(initialState, spreadRules, numGenerations)

  if (true) {
    println(s"grownPlants")
    val indices = grownPlants.map(gp => String.format("%3s", gp._1.toString)).mkString("|")
    val plants = grownPlants.map(gp => String.format("%3s", SpreadRule.boolToPlant(gp._2))).mkString("|")
    println(indices)
    println(plants)
  }

  // Answer
  val sumOfPotsWithPlants = grownPlants
    .collect {
      case (idx, true) => idx
    }
    .sum
  println(s"sumOfPotsWithPlants = ${sumOfPotsWithPlants}")

  // Part 2
  // Formula = 1173 + (generation - 152) * 8
  // Adds 8 more per generation for every generation starting at generation 152
}
