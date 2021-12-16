package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.tailrec

object day9 {

  type Panel = Map[(Int,Int),Int]
  type Point = ((Int,Int),Int)

  def loadPanel(lines:LazyList[String]):Panel = {
    lines
      .zipWithIndex
      .flatMap{(line,col)=>
        line
          .split("")
          .toList.map(_.toInt)
          .zipWithIndex
          .map{(num, row) => ((row,col),num)}
      }.toMap
  }

  def isLow(point:Point, panel:Panel):Boolean = {
    val ((row,col), pointVal) = point
    val neighbours = List(
      (row-1, col),
      (row, col-1),
      (row, col+1),
      (row+1, col)
    ).flatMap(panel.get)
    neighbours.forall(x => x > pointVal)
  }

  def findLows(panel:Panel):List[Point] = panel.filter{point => isLow(point, panel)}.toList

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val lines = input.split("\n").to(LazyList)
    val panel = loadPanel(lines)
    val part1 = findLows(panel).map(_._2 +1).sum
    Console[F].println((part1, 0)) *> Applicative[F].pure(ExitCode.Success)




}
