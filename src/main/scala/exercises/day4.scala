package exercises

import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.implicits.*
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.apply.catsSyntaxApply
import cats.{Applicative, ApplicativeError}

import scala.annotation.unused


object day4 {

  case class Checkable(values:NonEmptyList[(Int, Boolean)]){
    val indexed:NonEmptyMap[Int,(Int,Boolean)] = values.zipWithIndex.map{(item, i) => (i,item)}.toNem
    lazy val checked:List[Int] = values.filter(_._2).map(_._1)
    lazy val unchecked:List[Int] = values.filter(!_._2).map(_._1)
    val isChecked:Boolean = values.forall(_._2)
    def draw(num:Int):Checkable = Checkable(values.map{(x, isActive) => if (x==num) (x, true) else (x, isActive)})
    def apply(num:Int):Option[(Int,Boolean)] = indexed(num)
  }



  case class Board(rows:NonEmptyList[Checkable]) {
    val columns: NonEmptyList[Checkable] =
      val nColumns = rows.head.values.size
      val columns = (0 until nColumns).map(i => Checkable(rows.map(ch=>ch(i).get))).toList
      NonEmptyList.fromListUnsafe(columns)

    def draw(num:Int):Board = Board(rows.map(ch => ch.draw(num)))

    lazy val unchecked: List[Int] = rows.toList.flatMap(_.unchecked)
    lazy val checked:List[Int] = rows.toList.flatMap(_.checked)

    val isChecked:Boolean = rows.exists(_.isChecked) || columns.exists(_.isChecked)

  }

  object Board{
    def fromString(strBlock:String):Board =
      val rows = strBlock
        .split("\n")
        .map(lineStr => lineStr.trim.split(" +").map(num => (num.toInt, false)))
        .map(rowVals => Checkable(NonEmptyList.fromListUnsafe(rowVals.toList))).toList
      Board(NonEmptyList.fromListUnsafe(rows))
  }

  def loadGame(input:String):(List[Int], List[Board]) = {
    val Array(h, xs*) = input.split("\n\n")
    val boards = xs.map(Board.fromString).toList
    (h.split(",").map(_.toInt).toList, boards)
  }

  type ConditionCheck = (List[Board], List[Board], Int) => Option[Int]

  def firstChecked(@unused oldBoards:List[Board], newBoards:List[Board], n:Int):Option[Int] = {
    newBoards.filter(_.isChecked) match {
      case Nil => None
      case x::_ => Some(x.unchecked.sum*n)
    }
  }

  def lastChecked(oldBoards:List[Board], newBoards:List[Board], n:Int):Option[Int] = {
    if (newBoards.exists(!_.isChecked)){
      None
    }else{
      oldBoards.filter(!_.isChecked) match {
        case x::Nil => Some(x.draw(n).unchecked.sum*n)
        case _ => None
      }
    }
  }

  def nextNum(num:Int, endCondition:ConditionCheck):State[List[Board], Option[Int]] = State{ boards =>
    val nextBoards = boards.map(_.draw(num))
    val result = endCondition(boards, nextBoards, num)
    (nextBoards, result)
  }

  def execute(input:List[Int], endCondition:ConditionCheck):State[List[Board], Option[Int]] ={
    input match {
      case scala.Nil => State.get.map(_ => scala.None)
      case x::xs => for {
        anyWinner <- nextNum(x, endCondition)
        current <- State.get
      } yield { anyWinner match {
        case None => execute(xs, endCondition).runA(current).value
        case Some(result) => Some(result)
      }
      }
    }
  }

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val (gameInput:List[Int], boards:List[Board]) = loadGame(input)
    val part1 = execute(gameInput, firstChecked).runA(boards).value
    val part2 = execute(gameInput, lastChecked).runA(boards).value

    Console[F].println((part1.get, part2.get)) *> Applicative[F].pure(ExitCode.Success)



}
