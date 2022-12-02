import scala.io.Source

object Day1:
  val filename = "inputs/day1/input.txt"
  val topNElves = 3

  def findHighestCalories: Unit =
    val inputLines = Source.fromFile(filename).getLines.toList
    val calorieSums = inputLines.foldLeft(List[Int]())((result, line) =>
      (result, line) match {
        case (_, "")              => 0 :: result
        case (Nil, _)             => List(line.toInt)
        case (current :: sums, _) => (current + line.toInt) :: sums
      }
    )
    val sortedCalorieSums = calorieSums.sorted.reverse
    val highestCalories = sortedCalorieSums.head
    val topNCalories = sortedCalorieSums.slice(0, topNElves).sum

    println(s"Most calories of a single elf: ${highestCalories}")
    println(s"Most calories of ${topNElves} elves: ${topNCalories}")
