package cc.taibel.aoc
package year2024

import scala.io.Source

object day01:
  @main
  def main(): Unit =
    val input = Source.fromInputStream(getClass.getResourceAsStream("day01")).getLines().toSeq
    val splitted = input.map(_.split("""\s+""", 2).map(_.toInt).toSeq)
    val leftList = splitted.map(_.head).sorted
    val rightList = splitted.map(_.last).sorted

    // Part01
    println(leftList.zip(rightList).map(el => (el._1 - el._2).abs).sum)

    //Part02
    val occurrences = rightList.groupMapReduce(identity)(_ => 1)(_ + _)
    println(leftList.map(el => el * occurrences.getOrElse(el, 0)).sum)
