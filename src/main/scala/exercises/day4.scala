package exercises

import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.implicits.*
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.apply.catsSyntaxApply
import cats.{Applicative, ApplicativeError}


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

    def anyRowComplete: Boolean = rows.exists(_.isChecked)
    def anyColumnComplete: Boolean = columns.exists(_.isChecked)
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

  def firstChecked(boards:List[Board]):Option[Board] = {
    boards.filter(_.isChecked) match {
      case Nil => None
      case x::_ => Some(x)
    }
  }
  
  def nextNum(num:Int, endCondition:List[Board] => Option[Board]):State[List[Board], Option[Board]] = State{ boards =>
    val nextBoards = boards.map(_.draw(num))
    val winner = endCondition(nextBoards)
    (nextBoards, winner)
  }

  def execute(input:List[Int], endCondition:List[Board] => Option[Board]):State[List[Board], Option[(Board, Int)]] ={
    input match {
      case scala.Nil => State.get.map(_ => scala.None)
      case x::xs => for {
        anyWinner <- nextNum(x, endCondition)
        current <- State.get
      } yield { anyWinner match {
        case None => execute(xs, endCondition).runA(current).value
        case Some(w) => Some((w,x))
      }
      }
    }
  }

  def computeResult(maybeWinner:Option[(Board, Int)]):Option[Int] = maybeWinner match {
    case None => None
    case Some((b:Board, n)) => Some((b.unchecked.sum*n))
  }

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val (gameInput:List[Int], boards:List[Board]) = loadGame(input)
    val part1 = computeResult(execute(gameInput, firstChecked).runA(boards).value)
    val part2 = computeResult(execute(gameInput, firstChecked).runA(boards).value)

    Console[F].println((part1, part2)) *> Applicative[F].pure(ExitCode.Success)



}
