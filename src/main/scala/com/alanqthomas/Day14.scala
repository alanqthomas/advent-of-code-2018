package com.alanqthomas

object Day14 extends App {
  val t0 = System.nanoTime()

  // Part 1
  def findRecipes(recipes: Array[Int], numRecipes: Int): Array[Int] = {
    def rec(elf1Idx: Int, elf2Idx: Int, lastIdx: Int): Array[Int] = {
      if (lastIdx >= numRecipes + 10) recipes.slice(lastIdx - 10, lastIdx)
      else {
        val sum = recipes(elf1Idx) + recipes(elf2Idx)

        var currentIdx = lastIdx
        sum.toString.map(_.toString.toByte).foreach(d => {
          recipes(currentIdx) = d
          currentIdx += 1
        })

        def getNewIdx(idx: Int): Int = (idx + recipes(idx) + 1) % currentIdx

        val newElf1Idx = getNewIdx(elf1Idx)
        val newElf2Idx = getNewIdx(elf2Idx)

        rec(newElf1Idx, newElf2Idx, currentIdx)
      }
    }

    recipes(0) = 3
    recipes(1) = 7

    rec(0, 1, 2)
  }

  val numRecipes = 47801
  var recipes = Array.ofDim[Int](21000000)

  // Answer
//  val improvedRecipes = findRecipes(recipes, numRecipes)
//  println(s"Improved Recipes = ${improvedRecipes.mkString}")

  def findRecipesBeforeInput(recipes: Array[Int], recipeInput: String): Int = {
    def rec(elf1Idx: Int, elf2Idx: Int, length: Int): Int = {

      if (length >= 6 && recipes.slice(length - 6, length).mkString == recipeInput) length - 6
      else if (length >= 7 && recipes.slice(length - 7, length - 1).mkString == recipeInput) length - 7
      else {
        val sum = recipes(elf1Idx) + recipes(elf2Idx)

        var currentIdx = length
        sum.toString.map(_.toString.toByte).foreach(d => {
          recipes(currentIdx) = d
          currentIdx += 1
        })

        def getNewIdx(idx: Int): Int = (idx + recipes(idx) + 1) % currentIdx

        val newElf1Idx = getNewIdx(elf1Idx)
        val newElf2Idx = getNewIdx(elf2Idx)

        rec(newElf1Idx, newElf2Idx, currentIdx)
      }
    }

    recipes(0) = 3
    recipes(1) = 7

    rec(0, 1, 2)
  }

  val recipesBeforeInput = findRecipesBeforeInput(recipes, "047801")
  println(s"Recipes before input = $recipesBeforeInput")

  val t1 = System.nanoTime()
  println(s"Run time: ${(t1 - t0) / 1000000}ms")
}
