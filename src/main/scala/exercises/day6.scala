package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.tailrec

object day6 {

  case class Fish(timer:Int){
    def nextDay:List[Fish] = {
      if (timer>0){
        List(Fish(timer-1))
      }else{
        List(Fish(timer=6), Fish(timer=8))
      }
    }
  }




  @tailrec
  def computeFishPopulation(days:Int, currentFishes:List[Fish]):Int = {
    days match {
      case 0 => currentFishes.length
      case _ => computeFishPopulation(days-1, currentFishes.flatMap(f => f.nextDay))
    }
  }


  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val initialPopulation = input.split("\n").head.split(",").map(_.toInt).toList.map(n => Fish(n))
    val part1 = computeFishPopulation(80, initialPopulation)
//    val part2 = computeFishPopulation(256, initialPopulation) // Too slow
    Console[F].println((part1, 0)) *> Applicative[F].pure(ExitCode.Success)


}
