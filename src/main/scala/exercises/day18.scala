package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object day18 {

  val testString = "[[[[[9,8],1],2],3],4]"
  val numPattern = raw"(\\d+)".r

  trait Tree
  case class Node(value:Int) extends Tree
  case class Branch(l:Tree, r:Tree) extends Tree


  def parseNext(next:List[String], stack:List[Tree]):Tree = {
    println(next)
    next match {
      case ""::Nil => stack.head
      case ","::xs => Branch(stack.head, parseNext(xs, stack.tail))
      case numPattern(n)::xs => parseNext(xs, Node(n.toInt)::stack)
      case "]"::xs =>
        val i::j::other = stack
        parseNext(next, Branch(i,j)::other)
      case "["::xs =>  parseNext(xs, stack)
      case _ => stack.head
      //    case "["::xs => parseNext(xs, stack)
      //    case "["::numPattern(n)::","::xs => Branch(Node(n.toInt), parseNext(xs, unclosed))
      //    case ","::numPattern(n)::"]"::xs => Branch()
    }

  }

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val lines = input.split("\n").to(LazyList)
    Console[F].println((0, 0)) *> Applicative[F].pure(ExitCode.Success)




}
