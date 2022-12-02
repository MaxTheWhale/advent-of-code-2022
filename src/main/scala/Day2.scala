import scala.io.Source

object Day2:
  val filename = "inputs/day2/input.txt"

  enum Shape:
    case Rock, Paper, Scissors

  enum Outcome:
    case Win, Lose, Draw

  val outcomePoints: Map[Outcome, Int] =
    Map(Outcome.Win -> 6, Outcome.Draw -> 3, Outcome.Lose -> 0)

  val shapePoints: Map[Shape, Int] =
    Map(Shape.Rock -> 1, Shape.Paper -> 2, Shape.Scissors -> 3)

  val oppenentCodes: Map[String, Shape] =
    Map("A" -> Shape.Rock, "B" -> Shape.Paper, "C" -> Shape.Scissors)

  val playerCodes: Map[String, Shape] =
    Map("X" -> Shape.Rock, "Y" -> Shape.Paper, "Z" -> Shape.Scissors)

  val playerOutcomes: Map[String, Outcome] =
    Map("X" -> Outcome.Lose, "Y" -> Outcome.Draw, "Z" -> Outcome.Win)

  def calculateScore(opponentMove: Shape, playerMove: Shape): Int = {
    val shapeScore = shapePoints(playerMove)
    val outcome = (opponentMove, playerMove) match {
      case (Shape.Rock, Shape.Paper)     => Outcome.Win
      case (Shape.Paper, Shape.Scissors) => Outcome.Win
      case (Shape.Scissors, Shape.Rock)  => Outcome.Win
      case (Shape.Rock, Shape.Scissors)  => Outcome.Lose
      case (Shape.Paper, Shape.Rock)     => Outcome.Lose
      case (Shape.Scissors, Shape.Paper) => Outcome.Lose
      case (_, _)                        => Outcome.Draw
    }
    shapeScore + outcomePoints(outcome)
  }

  def chooseMove(opponentMove: Shape, playerOutcome: Outcome): Shape = {
    (opponentMove, playerOutcome) match {
      case (Shape.Rock, Outcome.Win)      => Shape.Paper
      case (Shape.Paper, Outcome.Win)     => Shape.Scissors
      case (Shape.Scissors, Outcome.Win)  => Shape.Rock
      case (Shape.Rock, Outcome.Lose)     => Shape.Scissors
      case (Shape.Paper, Outcome.Lose)    => Shape.Rock
      case (Shape.Scissors, Outcome.Lose) => Shape.Paper
      case (_, Outcome.Draw)              => opponentMove
    }
  }

  def rockPaperScissors: Unit =
    val inputLines = Source.fromFile(filename).getLines.toList
    val totalScore =
      inputLines.foldLeft(0)((score, line) =>
        val lineTokens = line.split(" ").toList
        val moveScore = lineTokens match {
          case opponentCode :: playerCode :: _ =>
            calculateScore(oppenentCodes(opponentCode), playerCodes(playerCode))
          case _ => 0
        }
        moveScore + score
      )
    val totalScorePart2 =
      inputLines.foldLeft(0)((score, line) =>
        val lineTokens = line.split(" ").toList
        val moveScore = lineTokens match {
          case opponentCode :: playerCode :: _ => {
            val opponentMove = oppenentCodes(opponentCode)
            val playerMove =
              chooseMove(opponentMove, playerOutcomes(playerCode))
            calculateScore(opponentMove, playerMove)
          }
          case _ => 0
        }
        moveScore + score
      )

    println(s"Total score for strategy (part 1): ${totalScore}")
    println(s"Total score for strategy (part 2): ${totalScorePart2}")
