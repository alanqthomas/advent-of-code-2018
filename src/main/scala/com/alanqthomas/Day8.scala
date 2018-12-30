package com.alanqthomas

import scala.collection.mutable.ListBuffer

object Day8 extends App {
  val file = Common.getLinesFromFile("day8-input.txt")

  val license = file.head.split(" ").toList

  case class Node(metadata: List[Int], children: List[Node])
  case class ExtractInfo(metadata: List[Int], numChildren: Int, newLicense: List[String])

  val headerSize = 2

  // Part 1
  def extractNodes(fullLicense: List[String]): Node = {
    def rec(license: List[String]): (Node, Int) = {
      val numChildren = license(0).toInt
      val numMetadata = license(1).toInt

      var children: ListBuffer[Node] = ListBuffer()
      val numRemovedFromChildren = {
        if (numChildren == 0) 0
        else {
          (0 until numChildren).toList.foldLeft(0) {
            case (numRemoved, _) =>
              val (node, numRemovedFromChildren) = rec(license.drop(headerSize + numRemoved))
              children += node
              numRemoved + numRemovedFromChildren
          }
        }
      }

      val numRemovedAndHeader = headerSize + numRemovedFromChildren
      val totalRemoved = numRemovedAndHeader + numMetadata

      val metadata = license.slice(numRemovedAndHeader, numMetadata + numRemovedAndHeader).map(_.toInt)

      (Node(metadata, children.toList), totalRemoved)
    }

    rec(fullLicense)._1
  }

  def getAllMetadata(node: Node): List[Int] = {
    if (node.children.isEmpty) node.metadata
    else {
      node.metadata ++ node.children.flatMap(getAllMetadata)
    }
  }

  val root = extractNodes(license)

  // Answer
  val checksum = getAllMetadata(root).sum
  println(s"checksum = ${checksum}")

  // Part 2
  def getNodeValue(node: Node): Int = {
    if (node.children.isEmpty) node.metadata.sum
    else {
      val childrenValues = node.children.map(getNodeValue)
      val children = ((1 to node.children.length) zip childrenValues).toMap

      val values = node.metadata.map(m => children.getOrElse(m, 0))

      values.sum
    }
  }

  // Answer
  val rootValue = getNodeValue(root)
  println(s"rootValue = ${rootValue}")
}
