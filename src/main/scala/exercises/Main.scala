package exercises

import cats.effect.*
import cats.implicits.*
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp

object AOC2021App extends CommandIOApp(
  name = "aoc2021",
  header = "Advent of Code 2021 CLI...or something",
  version = "0.0.1"
) {

  case class Exercise(n:Int)
  case class InputFile(path:String)
  case class InputString(input:String)

  val exOpts: Opts[Exercise] = Opts.argument[Int](metavar = "exercise_number").map(Exercise.apply)

  val fileOpt: Opts[InputFile] = Opts.option[String]("file", short = "f", help="Resources file to use as input").map(InputFile.apply)

  val inputStrOpt: Opts[InputString] = Opts.option[String]("input", short="i", help="Direct input").map(InputString.apply)


  def runExercise(exNumber:Int, input:String):IO[ExitCode] = exNumber match {
    case 1 => day1.solve(input)
    case 2 => day2.solve(input)
    case 3 => day3.solve(input)
    case 4 => day4.solve(input)
    case 5 => day5.solve(input)
    case 6 => day6.solve(input)
    case 7 => day7.solve(input)
    case _ => IO.println(s"Invalid exNumber $exNumber") *> IO(ExitCode.Error)
  }

  override def main: Opts[IO[ExitCode]] =
    ((fileOpt orElse inputStrOpt), exOpts).tupled.map{
      case (InputFile(f), Exercise(n)) =>
        IO.println(s"exercise $n input file $f")  *> aux.load(f).use{ s=> runExercise(n, aux.asInput(s))}
      case (InputString(s), Exercise(n)) =>
        IO.println(s"exercise $n input string $s")  *> runExercise(n, s)
    }

}