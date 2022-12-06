import scala.io.Source

object Day6:
  val filename = "inputs/day6/input.txt"
  val markerLength = 4
  val messageLength = 14

  def isAllUnique(sequence: String): Boolean = {
    sequence.distinct.length == sequence.length
  }

  def findFirstMarker(sequence: String, markerLength: Int): Int = {
    val indices: Range = (0 to sequence.length)
    val result = indices.find(index => {
      val substring = sequence.slice(index, index + markerLength)
      isAllUnique(substring)
    })
    result match {
      case Some(markerIndex) => markerIndex
      case _                 => -1
    }
  }

  def findMarkers: Unit = {
    val input = Source.fromFile(filename).mkString

    val firstMarkerIndex = findFirstMarker(input, markerLength)
    val processedChars = firstMarkerIndex + markerLength

    val firstMessageIndex = findFirstMarker(input, messageLength)
    val processedMessageChars = firstMessageIndex + messageLength

    println(s"First marker found after: ${processedChars} characters")
    println(s"First message found after: ${processedMessageChars} characters")
  }
