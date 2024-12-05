package cc.taibel.aoc
package year2024

import scala.io.Source

object day02:
  @main
  def day02Main(): Unit =
    val input = Source.fromInputStream(getClass.getResourceAsStream("day02")).getLines().toSeq
    val reports = input.map(_.split(" ").map(_.toInt).toSeq)

    // part 01
    println(reports.count(processReport))

    // part 02
    print(reports.map(reportVarieties).count(_.exists(processReport)))

  def processReport(report: Seq[Int]): Boolean =
    val sliding = report.sliding(2, 1).toSeq
    val asc = sliding.head.head < sliding.head.last
    sliding.forall(el => (1 to 3 contains (el.head - el.last).abs) && (if asc then el.head < el.last else el.last < el.head))

  def reportVarieties(report: Seq[Int]): Seq[Seq[Int]] =
    report +: report.indices.map(i => report.patch(i, Nil, 1))
    