import java.io._

import scala.annotation.tailrec

object ProblemC {
  def main(args: Array[String]): Unit = processStdInOut()

  def processStdInOut(): Unit = {
    val src = io.Source.fromInputStream(System.in)
    try {
      val bw = new BufferedWriter(new OutputStreamWriter(System.out));
      try {
        val lines = src.getLines()
        processLines(lines, bw)
      } finally {
        bw.flush()
        bw.close()
      }
    } finally {
      src.close();
    }
  }

  // Standard code above, custom code below

  case class Signal(val x: Int, val y: Int)

  def processLines(lines: Iterator[String], bw: BufferedWriter): Unit = {
    val line = lines.next()
    val strings = line.split(' ')
    val n = strings(0).toInt
    val m = strings(1).toInt
    val k = strings(2).toInt
    val signals = Array.ofDim[Signal](k)
    for (i <- 0 until k) {
      val row = lines.next()
      val coords = row.split(' ').map(_.toInt)
      signals(i) = Signal(coords(0), coords(1))
    }

    val firstTimeEachSignalIsHit = solve(n, m, k, signals)
    firstTimeEachSignalIsHit.map {
      case Some(tme) => tme.toString
      case _ => "-1"
    }.foreach { str =>
      bw.write(str)
      bw.newLine()
    }
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def lcm(a: Int, b: Int): Long = a.toLong * b / gcd(a, b)

  def getShortestTime(n: Int, m: Int, sig: Signal, maxTime: Long): Option[Long] = {
    // Consider the four reflections of the point(x, y) about the centre line of a 2n by 2m box
    // i.e. n +/- (n - x), m +/- (m - y)
    def timeToReachSignalInBlock(tStartOfBlock: Long): Option[Long] = {
      def isSignalReached(t: Long): Boolean = {
        val yOffsetFromBlock = t % (2 * m)
        (yOffsetFromBlock == sig.y) || (yOffsetFromBlock == 2 * m - sig.y)
      }
      val xVals = List(tStartOfBlock + sig.x, tStartOfBlock + 2 * n - sig.x)
      val ttr = xVals.filter(isSignalReached).headOption
      ttr
    }

    // x and y must be congruent modulo 2:
    if ((sig.x % 2) != (sig.y % 2)) None else {
      (0L until maxTime by (2 * n)).map(timeToReachSignalInBlock).flatten.headOption
    }
  }

  def solve(n: Int, m: Int, k: Int, signals: Array[Signal]): Array[Option[Long]] = {
    if (n < m) {
      // Performance is better if we jump up by the larger of (n, m) so swap axes if n < m:
      def swapAxis(sig: Signal) = sig.copy(x = sig.y, y = sig.x)
      solve(m, n, k, signals.map(swapAxis))
    } else {
      val rayTime = lcm(n, m)
      val shortestTimes = signals.map(getShortestTime(n, m, _, rayTime))
      shortestTimes
    }
  }
}