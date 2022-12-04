import scala.io.Source

object Day3:
  val filename = "inputs/day3/input.txt"

  val priorityBaseLower = 1
  val priorityBaseUpper = 27

  def itemToPriority(item: Char): Int = {
    item match {
      case c if c >= 'a' && c <= 'z' => c.toInt - 'a'.toInt + priorityBaseLower
      case c if c >= 'A' && c <= 'Z' => c.toInt - 'A'.toInt + priorityBaseUpper
      case _                         => 0
    }
  }

  def findDuplicate(rucksack: String): Char = {
    val compartmentSize = rucksack.length / 2
    val compartmentA = rucksack.slice(0, compartmentSize)
    val compartmentB = rucksack.slice(compartmentSize, rucksack.length)
    val duplicate = compartmentA.find(item => compartmentB.contains(item))
    duplicate match {
      case Some(item) => item
      case _          => '0'
    }
  }

  def findGroupBadge(rucksacks: List[String]): Char = {
    rucksacks match {
      case a :: b :: c :: _ => {
        val badge = a.find(item => b.contains(item) && c.contains(item))
        badge match {
          case Some(item) => item
          case _          => '0'
        }
      }
      case _ => '0'
    }
  }

  def organiseRucksack: Unit = {
    val inputLines = Source.fromFile(filename).getLines.toList
    val duplicates = inputLines.map(findDuplicate)
    val priorites = duplicates.map(itemToPriority)

    val groups = inputLines.grouped(3).toList
    val badges = groups.map(findGroupBadge)
    val badgePriorites = badges.map(itemToPriority)

    println(s"Total priorites of duplicate items: ${priorites.sum}")
    println(s"Total priorites of group badges: ${badgePriorites.sum}")
  }
