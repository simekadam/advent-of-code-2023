import scala.collection.immutable.NumericRange

object Day6 {
  def main(args: Array[String]): Unit = {
    val lines = {
      scala.io.Source.fromFile(args.head).getLines.toVector
    }

    val t1 = System.currentTimeMillis()
    println("Part 1: "+solutionPart1(lines))
//    println("Part 2: "+solutionPart2(lines))
    println("Part 2: "+solutionPart2Math(lines))
    println("Duration: "+(System.currentTimeMillis() - t1))

  }

  private def solutionPart1(lines: IndexedSeq[String]): Long = {
    val numRows = lines.map(_.split(' ').tail.flatMap(_.trim.split(' ').map(_.trim).filterNot(_.isEmpty).map(_.toInt)))
    val t1 = System.currentTimeMillis()
    val result = numRows(0).indices.map{ index =>
        calculateIt(numRows(0)(index), numRows(1)(index))
      }.product
    println("Duration part1: "+(System.currentTimeMillis() - t1))
    result
  }

  private def solutionPart2(lines: IndexedSeq[String]): Long = {
    val numRows = lines.map(_.split(':').tail.head.replaceAll(" ", "").toLong)
      val durationMs = numRows(0)
      var options = 0
      (1L to durationMs).foreach { waitTimeMs =>
        if ((durationMs - waitTimeMs) * waitTimeMs > numRows(1)) {
          options += 1
        }
      }
      options
  }

  private def solutionPart2Math(lines: IndexedSeq[String]): Long = {
    val numRows = lines.map(_.split(':').tail.head.replaceAll(" ", "").toLong)
    val t1 = System.currentTimeMillis()
    val result = calculateIt(numRows(0), numRows(1))
    println("Duration part2: "+(System.currentTimeMillis() - t1))
    result

  }

  private def calculateIt(time: Long, distance: Long): Long = {
    // trunc(sqrt(t²-4d)) + 1
    Math.sqrt((Math.pow(time, 2) - 4 * distance)).floor.toLong + 1
  }
}