package cc.taibel.aoc
package year2024

import scala.annotation.tailrec
import scala.io.Source


object day12:
  private val directions = Seq(
    (1, 0),
    (0, 1),
    (-1, 0),
    (0, -1),
  )

  @main
  def day12main(): Unit =
    val input = Source.fromInputStream(getClass.getResourceAsStream("day12")).getLines()
    val potCoords = input.zipWithIndex.flatMap((row, y) => row.zipWithIndex.map((pot, x) => (x, y, pot)))
    val potTypes = potCoords.toSeq.groupMap(_._3)((x, y, _) => (x, y)).values.map(_.toSet)
    val potAreas = potTypes.flatMap(splitPots)
    val price = potAreas.map(area =>
      area.toSeq.map((x, y) => directions.count((offsetX, offsetY) => !area.contains((x + offsetX, y + offsetY))))
        .sum * area.size
    ).sum
    println(price)

    val price2 = potAreas.map(area =>
      directions.map((offsetX, offsetY) =>
        area.count((x, y) => !area.contains((x + offsetX, y + offsetY)) && (!area.contains(if offsetX == 0 then (x-1, y) else (x, y+1)) || area.contains(if offsetX == 0 then (x-1, y+offsetY) else (x+offsetX, y+1))))).sum * area.size
    ).sum
    println(price2)

  private def splitPots(pots: Set[(Int, Int)]): Seq[Set[(Int, Int)]] =
    def iterateArea(pots: Set[(Int, Int)], current: (Int, Int)): (Set[(Int, Int)], Set[(Int, Int)]) =
      val newPots = pots - current
      directions
        .map((offsetX, offsetY) => (current._1 + offsetX, current._2 + offsetY))
        .filter(newPots.contains)
        .foldLeft((newPots, Set(current): Set[(Int, Int)]))((former, current) =>
          val result = iterateArea(former._1, current)
          (former._1 -- result._2, former._2 ++ result._2)
        )

    @tailrec
    def inner(pots: Set[(Int, Int)], result: Seq[Set[(Int, Int)]]): Seq[Set[(Int, Int)]] =
      if pots.isEmpty then result
      else
        val (newPots, area) = iterateArea(pots, pots.head)
        inner(newPots, result :+ area)
    inner(pots, Seq())
