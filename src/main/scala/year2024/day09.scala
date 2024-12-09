package cc.taibel.aoc
package year2024

import scala.annotation.tailrec
import scala.io.Source

class FsObject
case class Free(size: Int) extends FsObject
case class File(size: Int, index: Int) extends FsObject:
  def calculateSum(pos: Int): (Long, Int) =
    val newPos = pos + size
    ((pos until newPos).sum.toLong * index, newPos)

object day09:

  @main
  def day09main(): Unit =
    val line = Source.fromInputStream(getClass.getResourceAsStream("day09")).getLines().next().map(_.asDigit)

    val elements: Seq[FsObject] = line.zipWithIndex.map((size, i) => if i % 2 == 0 then File(size, i / 2) else Free(size))

    // Part 01
    println(process(elements, defragmentPart1))

    // Part 02
    println(process(elements, defragmentPart2))


  def process(elements: Seq[FsObject], f: (Seq[FsObject], Int, Int, Long) => (Seq[FsObject], Long)): Long =
    @tailrec
    def inner(elements: Seq[FsObject], pos: Int = 0, checksum: Long = 0): Long =
      if elements.isEmpty then checksum
      else
        elements.head match
          case file: File =>
            val (newChecksum, newPos) = file.calculateSum(pos)
            inner(elements.tail, newPos, checksum + newChecksum)
          case Free(size) =>
            val (newElements, newChecksum) = f(elements.tail, pos, size, checksum)
            inner(newElements, pos + size, newChecksum)
    inner(elements)


  @tailrec
  private def defragmentPart1(remaining: Seq[FsObject], startPos: Int, size: Int, checksum: Long): (Seq[FsObject], Long) =
    if remaining.isEmpty || size == 0 then (remaining, checksum)
    else remaining.last match
      case _: Free => defragmentPart1(remaining.dropRight(1), startPos, size, checksum)
      case File(fileSize, index) =>
        val newPos = startPos + math.min(fileSize, size)
        val newChecksum = checksum + (startPos until newPos).sum.toLong * index
        if size >= fileSize then defragmentPart1(remaining.dropRight(1), newPos, size - fileSize, newChecksum)
        else (remaining.dropRight(1).appended(File(fileSize - size, index)), newChecksum)

  @tailrec
  private def defragmentPart2(remaining: Seq[FsObject], startPos: Int, size: Int, checksum: Long): (Seq[FsObject], Long) =
    if remaining.isEmpty || size <= 0 then (remaining, checksum)
    else
      val result = remaining.zipWithIndex.filter(_._1.isInstanceOf[File]).map((file, i) => (file.asInstanceOf[File], i)).findLast(_._1.size <= size)
      result match
        case None => (remaining, checksum)
        case Some((file, i)) =>
          val newRemaining = remaining.updated(i, Free(file.size))
          val (newChecksum, newPos) = file.calculateSum(startPos)
          defragmentPart2(newRemaining, newPos, size - file.size, checksum + newChecksum)
