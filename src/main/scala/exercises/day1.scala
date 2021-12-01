package exercises

import cats.effect.ExitCode
import cats.{Applicative, ApplicativeError}
import cats.effect.std.Console
import cats.syntax.apply.catsSyntaxApply

import scala.annotation.tailrec


object day1 {

  def solve[F[_]:Console:Applicative](input: String): F[ExitCode] =
    val nums = input.split("\n").to(LazyList).map(_.toInt)
    Console[F].println((part1(nums), part2(nums,3))) *> Applicative[F].pure(ExitCode.Success)

  @tailrec
  def deltas(l:LazyList[Int], accum:List[Int] = List()):List[Int] = {
    l match {
      case x#::y#::xs =>deltas(y#::xs, (y-x) +: accum)
      case x#::LazyList() => accum.reverse
      case LazyList() => accum.reverse
    }
  }

  def part1(l:LazyList[Int]):Int = deltas(l).count(_ > 0)


  def part2(l:LazyList[Int], w:Int):Int = deltas(l.sliding(w).map(_.sum).to(LazyList)).count(_>0)



}
