package cc.taibel.aoc
package year2023

import scala.annotation.tailrec
import scala.io.Source

object day01 {
  private val digitMapping = Map(
    "one" -> 1,
    "two" -> 2,
    "three" -> 3,
    "four" -> 4,
    "five" -> 5,
    "six" -> 6,
    "seven" -> 7,
    "eight" -> 8,
    "nine" -> 9,
  )

  @main
  def main(): Unit = {
    val input = Source.fromInputStream(getClass.getResourceAsStream("day01")).getLines().toSeq
    println(input.map(parseLine).sum)
    println(input.map(findRecursive(_)).sum)
  }

  private def parseLine(line: String): Int = {
    val digits = line.filter('0' to '9' contains _)
    s"${digits.head}${digits.last}".toInt
  }

  @tailrec
  private def findRecursive(line: String, pos: Int = 0, results: List[Int] = List()): Int = {
    val newElement = isDigit(line.charAt(pos)).orElse(digitMapping.find((key, _) => line.take(pos+1).endsWith(key)).map(_._2))
    val newList = results ++ newElement
    val newPos = pos + 1
    if newPos == line.length then {
      s"${newList.head}${newList.last}".toInt
    }
    else findRecursive(line, newPos, newList)
  }

  private def isDigit(char: Char): Option[Int] = char match {
    case char if '0' to '9' contains char => Some(char.asDigit)
    case _ => None
  }

}
