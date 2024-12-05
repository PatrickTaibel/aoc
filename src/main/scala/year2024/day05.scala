package cc.taibel.aoc
package year2024

import scala.io.Source

object day05:
  @main
  def day05main(): Unit =
    val lines = Source.fromInputStream(getClass.getResourceAsStream("day05")).getLines().toSeq
    val ruleDefintiions = lines.takeWhile(_ != "")
    val orderRules = ruleDefintiions.map{case s"$k|$v" => (k.toInt, v.toInt)}.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    val jobs = lines.drop(ruleDefintiions.size+1).map(_.split(",").map(_.toInt).toSeq)
    val (correct, incorrect) = jobs.partition(checkOrder(orderRules))

    // Part 01
    println(middleSum(correct))

    // Part 02
    val sorted = incorrect.map(_.sortWith(cmpLt(orderRules)))
    println(middleSum(sorted))

  private def checkOrder(rules: Map[Int, Seq[Int]])(job: Seq[Int]): Boolean =
    job.zipWithIndex.forall((page, i) =>
      if rules contains page then
        val rule = rules(page)
        job.take(i).forall(previous => !(rule contains previous))
      else true
    )

  private def middleSum(jobs: Seq[Seq[Int]]): Int = jobs.map(job => job(job.size / 2)).sum

  private def cmpLt(rules: Map[Int, Seq[Int]])(el1: Int, el2: Int): Boolean =
    if rules.contains(el1) then rules(el1).contains(el2)
    else if rules.contains(el2) then !rules(el2).contains(el1)
    else true
