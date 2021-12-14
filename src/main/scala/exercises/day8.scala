package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.tailrec

object day8 {

  case class Display(
                    U:Option[Char],
                    UL:Option[Char],
                    UR:Option[Char],
                    M:Option[Char],
                    LL:Option[Char],
                    LR:Option[Char],
                    L:Option[Char]
                    ){
    val isDone:Boolean = List(U, UL, UR, M, LL, LR, L).flatten.length == 7
  }

  def parse(line:String):(List[String],List[String]) = {
    line.split("\\|") match {
      case Array(x,y) => (x.trim.split(" ").toList, y.trim.split(" ").toList)
      case _ => (List(), List())
    }
  }

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val lines = input.split("\n")
    val part1 = lines.flatMap(ln => parse(ln)._2).count(i => List(7, 3,4,2).contains(i.length))
    val part2 = 0
    Console[F].println((part1, part2)) *> Applicative[F].pure(ExitCode.Success)




}
