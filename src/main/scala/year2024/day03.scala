package cc.taibel.aoc
package year2024

import scala.io.Source

object day03:
  private val matchRe = """mul\((\d{1,3}),(\d{1,3})\)""".r
  private val splitRe = """don't.*?(?:do(?!n't)|$)""".r

  @main
  def day03Main(): Unit =
    val input = Source.fromInputStream(getClass.getResourceAsStream("day03")).getLines().toSeq.mkString

    //Part 01
    println(processMul(input).sum)

    //Part 02
    println(splitRe.split(input).flatMap(processMul).sum)

  def processMul(input: String): Iterator[Int] =
    matchRe.findAllMatchIn(input).map(el => el.group(1).toInt * el.group(2).toInt)
