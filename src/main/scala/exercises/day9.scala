package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.tailrec

object day9 {

  type Coords = (Int,Int)
  type Point = (Coords,Int)
  type Panel = Map[Coords,Int]


  def loadPanel(lines:LazyList[String]):Panel = {
    lines
      .zipWithIndex
      .flatMap{(line,row)=>
        line
          .split("")
          .toList.map(_.toInt)
          .zipWithIndex
          .map{(num, col) => ((row,col),num)}
      }.toMap
  }

  def getNeighbours(c:Coords, panel:Panel):LazyList[Coords] = {
    val (row,col) = c
    LazyList(
      (row-1, col),
      (row, col-1),
      (row, col+1),
      (row+1, col)
    ).filter( x => panel.contains(x))
  }

  def isLow(point:Point, panel:Panel):Boolean = {
    val (coords, pointVal) = point
    val neighbours = getNeighbours(coords, panel).flatMap(panel.get)
    neighbours.forall(x => x > pointVal)
  }

  def findLows(panel:Panel):List[Point] = panel.filter{point => isLow(point, panel)}.toList

  def computeBasinArea(point:Coords, panel:Panel):Int = {
    def basinPoints(c:Coords, panel:Panel, visited:Set[Coords]):Set[Coords] = {
      val notVisited = getNeighbours(c, panel).filter{ c => !visited.contains(c) && panel.getOrElse(c, 9) < 9}.toSet
      if (notVisited.isEmpty) visited
      else{
        val nowVisited =  visited ++ notVisited
        notVisited.foldLeft(nowVisited){(z, neigh) =>z ++ basinPoints(neigh, panel, z)}
      }

    }
    basinPoints(point, panel, Set(point)).size
  }


  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val lines = input.split("\n").to(LazyList)
    val panel = loadPanel(lines)
    val lows = findLows(panel)
    val part1 = lows.map(_._2 +1).sum
    val basins = lows.map(_._1).map(computeBasinArea(_, panel))
    val part2 = basins.sorted(Ordering[Int].reverse).take(3).product
    Console[F].println((part1, part2)) *> Applicative[F].pure(ExitCode.Success)




}
