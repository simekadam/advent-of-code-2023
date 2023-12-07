import scala.collection.immutable.NumericRange

object Solution {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.fromFile(args.head).getLines.toVector

    println(solution(lines))
  }

  private def solution(lines: IndexedSeq[String]): Long = {
    val seedRanges = lines.head.split(':').last.trim.split(' ').map(_.trim.toLong).grouped(2).map { case Array(a, b) => a until a + b }

    val locationFinder = getLocationRanges(parseMaps(lines.drop(1)))_

    seedRanges.map { seedRange =>
      locationFinder(seedRange).map(_.min).min
    }.min
  }

  def parseMaps(lines: Seq[String]): List[Seq[(NumericRange[Long], Long)]] = {
    if (lines.isEmpty) { return Nil }

    val (mapLines, rest) = lines.dropWhile(_.isEmpty).span(_.nonEmpty)

    val map = mapLines.drop(1).map { l =>
      val Array(destination, origin, size) = l.split(' ').map(_.toLong)

      ((origin until origin + size), destination - origin)
    }.sortBy(_._1.head)

    map :: parseMaps(rest)
  }

  def getLocationRanges(maps: Seq[Seq[(NumericRange[Long], Long)]])(seeds: NumericRange[Long]): Seq[NumericRange[Long]] = {
    maps.foldLeft(Seq(seeds)) { (inRanges, outRanges) =>
      inRanges.flatMap { inRange =>
        (outRanges ++ gaps(inRange, outRanges.map(_._1))).map { case (outRange, offset) =>
          val intersection = intersect(inRange, outRange)
          intersection.start + offset until intersection.end + offset
        }.filter(_.nonEmpty)
      }
    }
  }

  def intersect(a: NumericRange[Long], b: NumericRange[Long]): NumericRange[Long] = {
    Math.max(a.start, b.start) until Math.min(a.end, b.end)
  }

  def gaps(a: NumericRange[Long], others: Seq[NumericRange[Long]]): Seq[(NumericRange[Long], Long)] = {
    (others.sliding(2).map { case Seq(a, b) =>
      a.end until b.start
    } ++ Seq(Long.MinValue until others.head.start, others.last.end until Long.MaxValue)).map { gap =>
      intersect(a, gap)
    }.filter(_.nonEmpty).map((_, 0L)).toSeq
  }
}