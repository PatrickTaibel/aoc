package cc.taibel.aoc
package year2024

import scala.io.Source


object day11:

  @main
  def day11main(): Unit =
    val stones = Source.fromInputStream(getClass.getResourceAsStream("day11")).getLines().next()
      .split(" ").groupMapReduce(_.toLong)(_ => 1L)(_ + _)

    println(iterateStones(stones).drop(25).next().values.sum)
    println(iterateStones(stones).drop(75).next().values.sum)

  private def iterateStones(start: Map[Long, Long]): Iterator[Map[Long, Long]] =
    Iterator.iterate(start)(
      _.toSeq.flatMap((stone, count) =>
          val size = (math.log10(stone).floor + 1).toInt
          stone match
            case 0 => Seq((1L, count))
            case stone if size % 2 == 0 =>
              val factor = math.pow(10, size / 2).toLong
              val first = stone / factor
              Seq((first, count), (stone - first * factor, count))
            case stone => Seq((stone * 2024, count))
          ).groupMapReduce(_._1)(_._2)(_ + _)
      )
