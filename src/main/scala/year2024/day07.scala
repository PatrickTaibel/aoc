package cc.taibel.aoc
package year2024

import scala.annotation.tailrec
import scala.io.Source

object day07:
  private val operationsPart1: Seq[(Long, Long) => Long] = Seq(_ + _, _ * _)
  private val operationsPart2: Seq[(Long, Long) => Long] = operationsPart1 :+ ((left, right) => s"$left$right".toLong)

  @main
  def day07main(): Unit =
    val input = Source.fromInputStream(getClass.getResourceAsStream("day07")).getLines().toSeq
    val processed = input.map(processEntry)

    // Part 01
    println(processed.filter(line => checkResult(line._1, line._2, operationsPart1)).map(_._1).sum)

    // Part 02
    println(processed.filter(line => checkResult(line._1, line._2, operationsPart2)).map(_._1).sum)

  private def processEntry(line: String): (Long, Seq[Long]) =
    val Array(result, operandString) = line.split(": ", 2)
    (result.toLong, operandString.split(" ").map(_.toLong))

  private def checkResult(expected: Long, operands: Seq[Long], operators: Seq[(Long, Long) => Long]): Boolean =
    def inner(remaining: Seq[Long], current: Long = 0): Boolean =
      if remaining.isEmpty then expected == current
      else if current > expected then false
      else
        operators.exists(op => inner(remaining.tail, op(current, remaining.head)))
    inner(operands)
