import scala.io.Source
import scala.util.matching.Regex

object Day5:
  val filename = "inputs/day5/input.txt"

  class Instruction(
      numCratesInput: Int,
      fromStackInput: Int,
      toStackInput: Int
  ) {
    val numCrates = numCratesInput
    val fromStack = fromStackInput - 1
    val toStack = toStackInput - 1
  }

  type Crates = List[List[Char]]

  def moveCrate(
      crates: Crates,
      fromIndex: Int,
      toIndex: Int
  ): Crates = {
    val fromStack = crates(fromIndex)
    val toStack = crates(toIndex)
    fromStack match {
      case topCrate :: newFromStack => {
        val newToStack = topCrate :: toStack
        crates
          .updated(fromIndex, newFromStack)
          .updated(toIndex, newToStack)
      }
      case _ => crates
    }
  }

  def moveCrates(
      crates: Crates,
      instruction: Instruction
  ): Crates = {
    val range = (1 to instruction.numCrates).toList
    range.foldLeft(crates)((currentCrates, _) =>
      moveCrate(currentCrates, instruction.fromStack, instruction.toStack)
    )
  }

  def moveCratesBatch(
      crates: Crates,
      instruction: Instruction
  ): Crates = {
    val fromStack = crates(instruction.fromStack)
    val toStack = crates(instruction.toStack)
    val cratesToMove = fromStack.slice(0, instruction.numCrates)
    val newFromStack = fromStack.slice(instruction.numCrates, fromStack.length)
    val newToStack = cratesToMove ++ toStack
    crates
      .updated(instruction.fromStack, newFromStack)
      .updated(instruction.toStack, newToStack)
  }

  def parseCratesFromInput(lines: List[String]): Crates = {
    val crateLines = lines.filter(line => line.contains('['))
    val crateChars =
      crateLines.map(line => line.drop(1).grouped(4).map(_.head).toList)
    crateChars.transpose.map(stack => stack.filter(c => c.isLetter))
  }

  def parseInstructionsFromInput(lines: List[String]): List[Instruction] = {
    val moveLines = lines.filter(line => line.contains("move"))
    val movePattern = """^move (\d*) from (\d*) to (\d*)$""".r

    moveLines.map(line => {
      line match {
        case movePattern(numCrates, fromStack, toStack) =>
          Instruction(numCrates.toInt, fromStack.toInt, toStack.toInt)
        case _ => Instruction(0, 1, 1)
      }
    })
  }

  def arrangeCrates: Unit = {
    val inputLines = Source.fromFile(filename).getLines.toList
    val crates = parseCratesFromInput(inputLines)
    val instructions = parseInstructionsFromInput(inputLines)

    val finalCrates =
      instructions.foldLeft(crates)((currentCrates, instruction) => {
        moveCrates(currentCrates, instruction)
      })
    val finalTops = finalCrates.map(_.head).mkString

    val finalCratesBatch =
      instructions.foldLeft(crates)((currentCrates, instruction) => {
        moveCratesBatch(currentCrates, instruction)
      })
    val finalTopsBatch = finalCratesBatch.map(_.head).mkString

    println(s"Final top crates after moving one at a time: ${finalTops}")
    println(s"Final top crates after moving in batches: ${finalTopsBatch}")
  }
