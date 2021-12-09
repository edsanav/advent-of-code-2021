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
  def computeFishPopulation(days:Int, population:Map[Int, BigInt]):BigInt = {
    if (days==0){
      population.values.sum
    }else{
      val newPop = population.map{(timer, amount) => (timer-1, amount)}
      val newGen = newPop.getOrElse(-1, BigInt(0)) // Amound that produced new offspring
      val nonAdults = newPop - -1 // Remove those that produced a new offspring
      val finalMap = nonAdults
        + (6->(nonAdults.getOrElse(6, BigInt(0)) + newGen)) // Old fishes in new reproductive cycle
        + (8->(nonAdults.getOrElse(8, BigInt(0)) + newGen)) // New offspring
      computeFishPopulation(days-1, finalMap)
    }
  }



  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val initialTimers = input.split("\n").head.split(",").map(_.toInt).toList
    val initialPopulation = initialTimers.groupMapReduce(p => p)(_ => BigInt(1))(_+_)
    val part1 = computeFishPopulation(80, initialPopulation)
    val part2 = computeFishPopulation(256, initialPopulation)
    Console[F].println((part1, part2)) *> Applicative[F].pure(ExitCode.Success)


}
