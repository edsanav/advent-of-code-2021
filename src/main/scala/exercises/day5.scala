package exercises

import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.implicits.*
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.apply.catsSyntaxApply
import cats.{Applicative, ApplicativeError}

import scala.util.matching.Regex

object day5 {

  case class Point(x:Int,y:Int)
  case class Line(init:Point, end:Point) {
    val isHorizontal:Boolean = init.y == end.y
    val isVertical: Boolean = init.x == end.x
    lazy val points:List[Point] = {
      val xPath = path(init.x, end.x)
      val yPath = path(init.y, end.y)
      if (xPath.length == 1) {
        List.fill(yPath.length)(xPath.head).zip(yPath).map(Point.apply.tupled)
      }else if (yPath.length == 1){
        xPath.zip(List.fill(xPath.length)(yPath.head)).map(Point.apply.tupled)
      }else{
        xPath.zip(yPath).map(Point.apply.tupled)
      }
    }
  }
  object Line {
    val pattern:Regex = raw"(\d+),(\d+) -> (\d+),(\d+)".r
    def fromInputStr(inputStr: String): Option[Line] = inputStr match {
      case pattern(x1,y1,x2,y2) => Some(Line(Point(x1.toInt,y1.toInt),Point(x2.toInt,y2.toInt)))
      case _ => None
    }
  }

  def path(a:Int, b:Int):List[Int] = if (a >= b) (a to b by -1).toList else (a to b).toList

  def countHotPoints(lines:LazyList[Line], pred:Line => Boolean = _ => true):Int = {
    lines
      .filter(pred)
      .flatMap(_.points)
      .groupMapReduce(p => p)(_ => 1)(_+_)
      .filter{(p,i) => i > 1}
      .toList
      .length
  }



  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val lines = input.split("\n").flatMap(Line.fromInputStr).to(LazyList)
    val part1 = countHotPoints(lines, l => l.isHorizontal || l.isVertical)
    val part2 = countHotPoints(lines)

    Console[F].println((part1, part2)) *> Applicative[F].pure(ExitCode.Success)


}
