package cc.taibel.aoc
package year2024

import scala.annotation.tailrec
import scala.io.Source


object day10:
  private val directions = Seq(
    (1, 0),
    (0, 1),
    (-1, 0),
    (0, -1),
  )

  @main
  def day10main(): Unit =
    val grid = Source.fromInputStream(getClass.getResourceAsStream("day10")).getLines().map(_.map(_.asDigit)).toSeq
    val levels = grid.zipWithIndex.flatMap((row, x) => row.zipWithIndex.map((level, y) => (x, y, level)))
    val trails = levels.filter(_._3 == 0).map((x, y, _) => calculateTrails((x, y), grid))
    println(trails.map(_.toSet.size).sum)
    println(trails.flatten.size)

  private def calculateTrails(pos: (Int, Int), grid: Seq[Seq[Int]]): Seq[(Int, Int)] =
    def inner(pos: (Int, Int), expectedLevel: Int): Seq[(Int, Int)] =
      val level = grid.lift(pos._1).flatMap(_.lift(pos._2))
      level match
        case None => Seq()
        case Some(level) if level != expectedLevel => Seq()
        case Some(level) if level == 9 => Seq(pos)
        case _ => directions.flatMap((offsetX, offsetY) => inner((pos._1 + offsetX, pos._2 + offsetY), expectedLevel + 1))
    inner(pos, 0)