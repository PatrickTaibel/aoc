package cc.taibel.aoc
package year2024

import scala.io.Source


object day13:

  @main
  def day13main(): Unit =
    val definitions = Source.fromInputStream(getClass.getResourceAsStream("day13")).getLines().grouped(4)
    val machines = definitions.toSeq.map(lines =>
      lines.take(3).mkString("--") match {
      case s"Button A: X+$xa, Y+$ya--Button B: X+$xb, Y+$yb--Prize: X=$xp, Y=$yp" =>
        ((xa.toInt, ya.toInt), (xb.toInt, yb.toInt), (xp.toLong, yp.toLong))
    })
    val machines2 = machines.map((a, b, prize) => (a, b, (prize._1 + 10000000000000L, prize._2 + 10000000000000L)))

    println(machines.flatMap(calculate).sum)
    println(machines2.flatMap(calculate).sum)

  private def calculate(a: (Int, Int), b: (Int, Int), prize: (Long, Long)): Option[Long] =
    val detA = prize._1 * b._2 - prize._2 * b._1
    val detB = prize._2 * a._1 - prize._1 * a._2
    val det = a._1 * b._2 - a._2 * b._1
    if det != 0 && detA % det == 0 && detB % det == 0 then
      Some(detA / det * 3 + detB / det)
    else None
