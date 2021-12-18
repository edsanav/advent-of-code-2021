package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.tailrec

object day18 {

  val testString = "[[[[[9,8],1],2],3],4]"

  trait Tree
  case class Node(value:Int) extends Tree
  case class Branch(l:Tree, r:Tree) extends Tree


  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val lines = input.split("\n").to(LazyList)
    Console[F].println((0, 0)) *> Applicative[F].pure(ExitCode.Success)




}
