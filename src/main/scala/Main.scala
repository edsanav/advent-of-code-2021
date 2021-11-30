import cats.effect.*
import cats.implicits.*

import com.monovore.decline.*
import com.monovore.decline.effect.*

object AOC2021App extends CommandIOApp(
  name = "aoc2021",
  header = "Advent of Code 2021 CLI...or something",
  version = "0.0.1"
) {

  case class Exercise(n:Int)
  case class InputFile(path:String)
  case class InputString(input:String)

  val exOpts: Opts[Exercise] = Opts.argument[Int](metavar = "exercise_number").map(Exercise(_))

  val fileOpt: Opts[InputFile] = Opts.option[String]("file", short = "f", help="Resources file to use as input").map(InputFile(_))

  val inputStrOpt: Opts[InputString] = Opts.option[String]("input", short="i", help="Direct input").map(InputString(_))


  override def main: Opts[IO[ExitCode]] =
    ((fileOpt orElse inputStrOpt), exOpts).tupled.map{
      case (InputFile(f), Exercise(n)) => IO.println(s"exercise $n input file $f")  *> IO(ExitCode.Success)
      case (InputString(s), Exercise(n)) => IO.println(s"exercise $n input string $s")  *> IO(ExitCode.Success)
    }

}