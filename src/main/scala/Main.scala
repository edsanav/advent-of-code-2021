import cats.effect._
import cats.implicits._

import com.monovore.decline._
import com.monovore.decline.effect._

object AOC2021App extends CommandIOApp(
  name = "aoc2021",
  header = "AOC2021 CLI...or something",
  version = "0.0.1"
) {

  val exOpts: Opts[String] =
    Opts.argument[String](metavar = "exercise")

  override def main: Opts[IO[ExitCode]] =
    (exOpts).map(s => IO.println(s"exercise is $s") *> IO(ExitCode.Success))

}