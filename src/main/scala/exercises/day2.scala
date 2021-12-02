package exercises

import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.apply.catsSyntaxApply
import cats.{Applicative, ApplicativeError}

import scala.annotation.tailrec


object day2 {

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val directions = input.split("\n").to(LazyList)
    val part1 = math.multiplyExact.tupled(move(directions, movement1))
    val part2 = math.multiplyExact.tupled(move(directions, movement2))
    Console[F].println((part1, part2)) *> Applicative[F].pure(ExitCode.Success)


  def toInstructions(l: LazyList[String]): LazyList[(String, Integer)] =
    l.map(_.split(" ")).flatMap {
      case Array(x, y) => Some((x, y.toInt))
      case _ => None
    }

  def movement1(operation: String, n: Int)(x: Int, y: Int, aim: Int): (Int, Int, Int) = {
    operation match {
      case "forward" => (x + n, y, aim)
      case "up" => (x, y - n, aim)
      case "down" => (x, y + n, aim)
    }
  }

  def movement2(operation: String, n: Int)(x: Int, y: Int, aim: Int): (Int, Int, Int) = {
    operation match {
      case "forward" => (x + n, y + aim * n, aim)
      case "up" => (x, y, aim - n)
      case "down" => (x, y, aim + n)
    }
  }


  def move(l: LazyList[String], movement: (String, Int) => (Int, Int, Int) => (Int, Int, Int)): (Int, Int) = {
    val finalPosition = toInstructions(l).foldLeft((0, 0, 0)) {
      case ((x, y, aim), (direction, n)) => movement(direction, n)(x, y, aim)
    }
    (finalPosition._1, finalPosition._2)
  }


}