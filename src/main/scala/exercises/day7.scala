package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

object day7 {

  def median(positions:List[Int]):Int = {
    // https://en.wikipedia.org/wiki/Geometric_median
    // For the 1-dimensional case, the geometric median coincides with the median.
    val sorted = positions.sortWith(_ < _)
    if (sorted.size % 2 == 1) sorted(sorted.size / 2)
    else{
      val (up, down) = sorted.splitAt(sorted.size/2)
      (up.last+down.head)/2
    }
  }

  def distances(positions:List[Int], center:Int):Int = positions.map(pos => math.abs(pos-center)).sum

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val positions = input.split("\n").head.split(",").map(_.toInt).toList
    val part1 = distances(positions, median(positions))
    Console[F].println((part1, 0)) *> Applicative[F].pure(ExitCode.Success)


}
