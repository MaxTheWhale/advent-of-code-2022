import scala.io.Source
import scala.util.matching.Regex

object Day4:
  val filename = "inputs/day4/input.txt"

  val priorityBaseLower = 1
  val priorityBaseUpper = 27

  def doRangesOverlapFully(rangeA: (Int, Int), rangeB: (Int, Int)): Boolean = {
    (rangeA, rangeB) match {
      case ((a, b), (c, d)) if a >= c && b <= d => true
      case ((a, b), (c, d)) if c >= a && d <= b => true
      case _                                    => false
    }
  }

  def doRangesOverlap(rangeA: (Int, Int), rangeB: (Int, Int)): Boolean = {
    (rangeA, rangeB) match {
      case ((a, b), (c, d)) if b >= c && a <= d => true
      case ((a, b), (c, d)) if c >= b && d <= a => true
      case _                                    => false
    }
  }

  def getRangesFromLine(line: String): ((Int, Int), (Int, Int)) = {
    val inputPattern: Regex = """^(\d*)-(\d*),(\d*)-(\d*)$""".r
    line match {
      case inputPattern(a, b, c, d) => ((a.toInt, b.toInt), (c.toInt, d.toInt))
      case _                        => ((0, 0), (0, 0))
    }
  }

  def findOverlaps: Unit = {
    val inputLines = Source.fromFile(filename).getLines.toList
    val ranges = inputLines.map(getRangesFromLine)
    val fullOverlaps = ranges.filter(doRangesOverlapFully)
    val overlaps = ranges.filter(doRangesOverlap)

    println(
      s"Number of pairs that are fully overlapping: ${fullOverlaps.length}"
    )
    println(s"Number of pairs that are overlapping: ${overlaps.length}")
  }
