package exercises

import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.apply.catsSyntaxApply
import cats.{Applicative, ApplicativeError}

import scala.annotation.tailrec


object day2 {

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val directions = input.split("\n").to(LazyList)
    val r1 = move(directions, (0,0), movement1, x => x)
    val r2 = move(directions, (0,0,0), movement2, (x,y,aim) => (x,y))

    Console[F].println((r1._1 * r1._2, r2._1 * r2._2)) *> Applicative[F].pure(ExitCode.Success)


  def toInstructions(l: LazyList[String]): LazyList[(String, Integer)] =
    l.map(_.split(" ")).flatMap {
      case Array(x, y) => Some((x, y.toInt))
      case _ => None
    }

  def movement1(operation: String, n: Int)(x: Int, y: Int): (Int, Int) = {
    operation match {
      case "forward" => (x + n, y)
      case "up" => (x, y - n)
      case "down" => (x, y + n)
    }
  }

  def movement2(operation: String, n: Int)(x: Int, y: Int, aim: Int): (Int, Int, Int) = {
    operation match {
      case "forward" => (x + n, y + aim * n, aim)
      case "up" => (x, y, aim - n)
      case "down" => (x, y, aim + n)
    }
  }


  def move[A] (l: LazyList[String], origin:A, movement: (String, Int) => A => A, out:A => (Int, Int)): (Int, Int) = {
    val finalPosition = toInstructions(l).foldLeft(origin) {case (z, (direction, n)) => movement(direction, n)(z)}
    out(finalPosition)
  }


}