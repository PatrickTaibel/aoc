package cc.taibel.aoc
package year2024

import scala.annotation.tailrec
import scala.io.Source

object day06:
  private val directions = Seq((-1, 0), (0, 1), (1, 0), (0, -1))

  @main
  def day06main(): Unit =
    val grid = Source.fromInputStream(getClass.getResourceAsStream("day06")).getLines().toSeq.map(_.toSeq)
    val startPos = grid.iterator.map(_.indexOf('^')).zipWithIndex.filter(_._1 >= 0).next()
    val startDir = (startPos._2, startPos._1, 0)
    val uniquePos = walk(grid, startDir).map((x, y, _) => (x, y))

    // Part 01
    println(uniquePos.size + 1)

    // Part 02
    val possibleBlocks = uniquePos - ((startPos._2-1, startPos._1))
    println(possibleBlocks.toSeq.map((x, y) => walk(grid.updated(x, grid(x).updated(y, '#')), startDir)).count(_.isEmpty))

  @tailrec
  private def walk(grid: Seq[Seq[Char]], pos: (Int, Int, Int), locations: Set[(Int, Int, Int)] = Set()): Set[(Int, Int, Int)] =
    val direction = directions(pos._3)
    val nextPos = (pos._1 + direction._1, pos._2 + direction._2, pos._3)
    val nextChar = grid.lift(nextPos._1).flatMap(_.lift(nextPos._2))
    nextChar match
      case Some('.') | Some('^') =>
        if locations.contains(nextPos) then Set()
        else walk(grid, nextPos, locations + nextPos)
      case Some('#') => walk(grid, pos.copy(_3=(pos._3 + 1) % directions.size), locations)
      case None => locations