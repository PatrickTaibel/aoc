package cc.taibel.aoc
package year2024

import scala.annotation.tailrec
import scala.io.Source


object day14:
  private val space = (101, 103)
  private val parts = Seq(
    (0 until space._1 / 2, 0 until space._2 / 2),
    (space._1 / 2 + 1 until space._1, 0 until space._2 / 2),
    (space._1 / 2 + 1 until space._1, space._2 / 2 + 1 until space._2),
    (0 until space._1 / 2, space._2 / 2 + 1 until space._2),
  )

  @main
  def day14main(): Unit =
    val input = Source.fromInputStream(getClass.getResourceAsStream("day14")).getLines()
    val robots = input.map {
      case s"p=$x,$y v=$vx,$vy" => (x.toInt, y.toInt, vx.toInt, vy.toInt)
    }.toSeq

    println(calculatePart01(robots))
    println(findPart02(robots))

  def calculatePart01(robots: Seq[(Int, Int, Int, Int)]): Long =
    val robotsEnd = robots.map(
      (x, y, vx, vy) => (calculateEndPos(x, vx, space._1, 100), calculateEndPos(y, vy, space._2, 100))
    ).groupMapReduce(identity)(_ => 1)(_ + _)
    parts.map(
      (xRange, yRange) =>
        robotsEnd.filter((pos, _) => xRange.contains(pos._1) && yRange.contains(pos._2)).values.sum.toLong
    ).product

  def findPart02(robots: Seq[(Int, Int, Int, Int)]): Int =
    calculateSteps(robots).zipWithIndex.find((robots, _) =>
      val positions = robots.map((x, y, _, _) => (x, y)).toSet
      positions.count((x, y) => positions.contains((x + 1, y)) && positions.contains((x + 2, y))) > 50
    ).map(_._2).get

  def calculateEndPos(pos: Int, velocity: Int, max: Int, steps: Int): Int =
    val newPos = (pos + velocity * steps) % max
    if newPos < 0 then max + newPos
    else newPos

  def calculateSteps(robots: Seq[(Int, Int, Int, Int)]): Iterator[Seq[(Int, Int, Int, Int)]] =
    Iterator.iterate(robots)(robots => robots.map((x, y, vx, vy) => (calculateEndPos(x, vx, space._1, 1), calculateEndPos(y, vy, space._2, 1), vx, vy)))