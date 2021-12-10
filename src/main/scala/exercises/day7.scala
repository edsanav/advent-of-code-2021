package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.tailrec

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
  def factorial(n:Int):Int = {
    @tailrec
    def go(num:Int, accum:Int):Int = num match {
      case 0 => accum
      case x => go(x-1, accum+x)
    }
    go(n, 0)
  }

  def distances(positions:List[Int], center:Int, adjust:Int=>Int = x => x):Int = positions.map(pos => adjust(math.abs(pos-center))).sum

  @tailrec
  def findCenter(positions:List[Int], startingPos: Int, adjust:Int=>Int = x => x):Int ={
    val initD = distances(positions, startingPos, adjust)
    val lowerD = distances(positions, startingPos-1, adjust)
    val upperD = distances(positions, startingPos+1, adjust)
    if (lowerD > initD){
      if (upperD > initD){
        startingPos
      }else{
        findCenter(positions, startingPos+1, adjust)
      }
    } else{
      findCenter(positions, startingPos-1, adjust)
    }
  }

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val positions = input.split("\n").head.split(",").map(_.toInt).toList
    val part1 = distances(positions, median(positions))
    val part2 = distances(positions, findCenter(positions, median(positions), factorial), factorial)
    Console[F].println((part1, part2)) *> Applicative[F].pure(ExitCode.Success)


}
