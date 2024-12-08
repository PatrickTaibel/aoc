package cc.taibel.aoc
package year2024

import scala.io.Source

object day08:

  @main
  def day08main(): Unit =
    val input = Source.fromInputStream(getClass.getResourceAsStream("day08")).getLines().toSeq
    val coords = input.zipWithIndex.flatMap((row, y) => row.zipWithIndex.filter(_._1 != '.').map((col, x) => (x, y, col)))
    val ranges = (input.head.indices, input.indices)
    val frequencies = coords.groupMap(_._3)((x, y, _) => (x, y))
    val combinations = frequencies.values.flatMap(_.combinations(2))
    println(calculate(combinations, ranges, Some(1)))
    println(calculate(combinations, ranges, None))

  def calculate(combinations: Iterable[Iterable[(Int, Int)]], ranges: (Range, Range), limit: Option[Int]): Int =
    val antinodes = combinations.flatMap(combination =>
      val (coord1, coord2) = (combination.head, combination.last)
      val distance = (coord1._1 - coord2._1, coord1._2 - coord2._2)
      positionsIterator(coord1, distance, ranges, limit) ++ positionsIterator(coord2, (-distance._1, -distance._2), ranges, limit)
    )
    antinodes.toSet.size

  def positionsIterator(start: (Int, Int), distance: (Int, Int), ranges: (Range, Range),
                        limit: Option[Int]): Iterator[(Int, Int)] =
    val iterator = Iterator.iterate(start)((x, y) => (x + distance._1, y + distance._2))
    val limited = if limit.isDefined then iterator.slice(1, limit.get + 1) else iterator
    limited.takeWhile((x, y) => ranges._1.contains(x) && ranges._2.contains(y))
