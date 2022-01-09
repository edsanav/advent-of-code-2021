package exercises

import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.implicits.*
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.syntax.apply.catsSyntaxApply
import cats.{Applicative, ApplicativeError}


object day4 {

  //Either a row or a column
  case class Checkable(values:NonEmptyList[(Int, Boolean)]){
    val indexed:NonEmptyMap[Int,(Int,Boolean)] = values.zipWithIndex.map{(item, i) => (i,item)}.toNem
    lazy val checked:List[Int] = values.filter(_._2).map(_._1)
    lazy val unchecked:List[Int] = values.filter(!_._2).map(_._1)
    val isChecked:Boolean = values.forall(_._2)

    //When drawing a number, new checkeable switching isActive in certain position if number is equal
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


  // State (State[S, A]) monad
  // represent functions of type S => (S, A) where S is the type of the state and a is the type of result.
  // Given its a monad, it follows monad laws. Also it has certain functionalities (State.get, State.set, etc)

  // Defined step to stop on first bingo.
  def checkFirstWin(n:Int):State[List[Board], Option[Int]] = State{ boards =>
    val newBoards = boards.map(_.draw(n))
    newBoards.filter(_.isChecked) match {
      case Nil => (newBoards, None)
      case x::_ => (newBoards, Some(x.unchecked.sum*n))
    }
  }

  // Defined step to stop on last bingo.
  def checkLastWin(n:Int):State[List[Board], Option[Int]] = State{ boards =>
    val newBoards = boards.map(_.draw(n))
    if (newBoards.exists(!_.isChecked)){
      (newBoards, None)
    }else{
      boards.filter(!_.isChecked) match {
        case x::Nil => (newBoards, Some(x.draw(n).unchecked.sum*n))
        case _ => (newBoards, None)
      }
    }
  }



  def execute(input:List[Int], step:Int => State[List[Board], Option[Int]]):State[List[Board], Option[Int]] ={
    input match {
      // If empty, result is None (no result)
      case scala.Nil => State.pure(None)
      // If not...
      case x::xs => for {
        // ... apply next number, get result (this gets a new state but for comprehension extracts it directly)...
        anyWinner <- step(x)
        // ... and get actual state (new boards) from the state monad
        currentBoards <- State.get
      } yield {
        // Then check result
        anyWinner match {
          // No final result reached yet, recursively call execute with remaining numbers and current state
          case None => execute(xs, step).runA(currentBoards).value
          // Final result reached, return final result
          case Some(result) => Some(result)
      }
      }
    }
  }

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val (gameInput:List[Int], boards:List[Board]) = loadGame(input)
    val part1 = execute(gameInput, checkFirstWin).runA(boards).value
    val part2 = execute(gameInput, checkLastWin).runA(boards).value

    Console[F].println((part1.get, part2.get)) *> Applicative[F].pure(ExitCode.Success)



}
