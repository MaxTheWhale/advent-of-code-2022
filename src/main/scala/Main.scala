@main def main(day: Int): Unit =
  println(s"Advent of Code 2022, day ${day}:")
  day match {
    case 1 => Day1.findHighestCalories
    case 2 => Day2.rockPaperScissors
    case 3 => Day3.organiseRucksack
    case 4 => Day4.findOverlaps
    case 5 => Day5.arrangeCrates
    case _ => println(s"Day ${day} not solved yet!")
  }
