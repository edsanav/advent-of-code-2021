package exercises

import cats.data.{NonEmptyList, NonEmptyMap, State}
import cats.effect.ExitCode
import cats.effect.std.Console
import cats.implicits.*
import cats.syntax.apply.catsSyntaxApply
import cats.{Applicative, ApplicativeError}
import exercises.day4.*

import scala.annotation.tailrec


object day4alt {

  // State (State[S, A]) monad
  // represent functions of type S => (S, A) where S is the type of the state and a is the type of result.
  // Given its a monad, it follows monad laws. Also it has certain functionalities (State.get, State.set, etc)

  // Defined step to stop on first bingo.
  def checkFirstWin(boards:List[Board], n:Int):(List[Board], Option[Int]) = {
    val newBoards = boards.map(_.draw(n))
    newBoards.filter(_.isChecked) match {
      case Nil => (newBoards, None)
      case x::_ => (newBoards, Some(x.unchecked.sum*n))
    }
  }

  // Defined step to stop on last bingo.
  def checkLastWin(boards:List[Board], n:Int):(List[Board], Option[Int]) = {
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
  
  type Step = (List[Board], Int) => (List[Board], Option[Int])

  @tailrec
  def execute(input:List[Int], boards:List[Board], step:Step):Option[Int] = {
    input match {
      // If empty, result is None (no result)
      case scala.Nil => None
      case n::futureNums => {
        val (nextBoards, anyWinner) = step(boards, n)
        anyWinner match {
          case None => execute(futureNums, nextBoards, step)
          case Some(result) => Some(result)
        }
      }
    } 
  }
  

  def solve[F[_] : Console : Applicative](input: String): F[ExitCode] =
    val (gameInput:List[Int], boards:List[Board]) = loadGame(input)
    val part1 = execute(gameInput, boards, checkFirstWin)
    val part2 = execute(gameInput, boards, checkLastWin)

    Console[F].println((part1.get, part2.get)) *> Applicative[F].pure(ExitCode.Success)



}
