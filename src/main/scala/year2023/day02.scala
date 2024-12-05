package cc.taibel.aoc
package year2023

import scala.io.Source

object day02 {
  private val gamePattern = """Game (?<id>\d+): (?<draws>.*)""".r
  private val drawPattern = """(?<draw>(?:\d+ [a-z]+(?:, )?)+)""".r
  private val grabPattern = """(\d+) ([a-z]+)""".r

  private val allowedMax = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14,
  )

  @main
  def day02Main(): Unit = {
    val input = Source.fromInputStream(getClass.getResourceAsStream("day01")).getLines().toSeq
    val games = input.map(parseLine)
    val validGames = games.filter(!_.draws.exists(_.cubes.exists((color, count) => count > allowedMax(color))))
    println(validGames.map(_.id).sum)
    println(games.map(_.draws.flatMap(_.cubes.toSeq).groupBy(_._1).map(_._2.map(_._2).max).product).sum)
  }

  private def parseLine(line: String): Game = {
    line match {
      case gamePattern(id, draws) => Game(id.toInt, parseDraws(draws))
      case _ => throw new Exception()
    }
  }

  private def parseDraws(draws: String): Seq[DrawResult] = {
    drawPattern.findAllMatchIn(draws).map(regexMatch => parseDraw(regexMatch.group("draw"))).toSeq
  }

  private def parseDraw(draw: String): DrawResult = {
    val cubes = draw.split(", ").map {
        case grabPattern(count, color) => (color, count.toInt)
        case _ => throw new Exception()
      }.toMap
    DrawResult(cubes)
  }
}

case class Game(id: Int, draws: Seq[DrawResult])

case class DrawResult(cubes: Map[String, Int])
