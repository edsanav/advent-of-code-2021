package exercises

import cats.Applicative
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*

import scala.annotation.tailrec

object day8 {

  def getCode(signals:List[Set[Char]]):Map[Set[Char], Int] = {
    val (defined, others) = signals.partition(x => List(7, 3,4,2).contains(x.size))
    val definedByLength = defined.map(ltt => (ltt.size,ltt)).toMap
    val (len5, len6) = others.partition(_.size == 5)
    val initial:Map[Int, Set[Char]] = Map(
      1 -> definedByLength(2),
      7 -> definedByLength(3),
      4 -> definedByLength(4),
      8 -> definedByLength(7)
    )
    val U = initial(7) -- initial(1)
    val six = len6.filter(s => (s & initial(1)).size == 1).head
    val zero = len6.filter(s => s!=six && (s & initial(4)).size == 3).head
    val nine = len6.filter(s => s!=six && s!=zero).head
    val five = len5.filter(s => (six & s) == s).head
    val two = len5.filter(s => s != five && (s & initial(1)).size == 1).head
    val three = len5.filter(s => s!=five && s!=two).head
    initial.map{(num, s) => (s, num)} ++ Map(
      zero ->0,
      five ->5,
      six->6,
      nine->9,
      two->2,
      three->3
    )
  }

  def parse(line:String):(List[Set[Char]],List[Set[Char]]) = {
    line.split("\\|") match {
      case Array(x,y) => (x.trim.split(" ").toList.map(_.toSet), y.trim.split(" ").toList.map(_.toSet))
      case _ => (List(), List())
    }
  }

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val lines = input.split("\n").to(LazyList).map(parse)
    val part1 = lines.flatMap(ln => ln._2).count(i => List(7, 3,4,2).contains(i.size))
    val part2 = lines
      .map{(signals, output) => (getCode(signals), output)}
      .map{(code, output)=> output.map(word => code(word).toString).mkString.toInt}.sum
    Console[F].println((part1, part2)) *> Applicative[F].pure(ExitCode.Success)




}
