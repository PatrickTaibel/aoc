package cc.taibel.aoc
package year2024

import scala.io.Source

object day04:
  @main
  def day04main(): Unit =
    val lines = Source.fromInputStream(getClass.getResourceAsStream("day04")).getLines().toSeq

    // Part 01
    val xPositions = getCharacterPositions(lines, 'X')
    val directions = for {
                            x <- -1 to 1
                            y <- -1 to 1 if x != 0 || y!= 0
                          } yield(x,y)
    println(xPositions.map((x, y) =>
      directions
        .filter((xMul, yMul) => checkDirectionValid(xMul, x, lines.size) && checkDirectionValid(yMul, y, lines.size))
        .count((xMul, yMul) => (1 to 3).map(index => lines(x+xMul*index)(y+yMul*index)).mkString == "MAS"
      )).sum)

    // Part 02
    val validRangePart2 = 1 until lines.size - 1
    val aPositions = getCharacterPositions(lines, 'A')
    println(
      aPositions
        .filter((x, y) => (validRangePart2 contains x) && (validRangePart2 contains y))
        .count((x, y) =>
          val diagonal1 = (lines(x-1)(y-1), lines(x+1)(y+1))
          val diagonal2 = (lines(x+1)(y-1), lines(x-1)(y+1))
          (diagonal1 == ('M', 'S') || diagonal1 == ('S', 'M')) && (diagonal2 == ('M', 'S') || diagonal2 == ('S', 'M'))
      )
    )

  private def getCharacterPositions(grid: Seq[String], char: Char): Seq[(Int, Int)] =
    grid.map(_.zipWithIndex).zipWithIndex.flatMap((row, x) => row.filter(_._1 == char).map((_, y) => (x, y)))

  private def checkDirectionValid(mulFactor: Int, x: Int, size: Int): Boolean =
    mulFactor == -1 && x >= 3 || mulFactor == 1 && x < size - 3 || mulFactor == 0
