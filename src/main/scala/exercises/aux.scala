package exercises

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.{Applicative, ApplicativeError}
import cats.syntax.applicativeError.*
import cats.effect.std.Console


import scala.io.{BufferedSource, Source}

object aux {

  //TODO abstract over F instead of having an explicit IO here
  def load(path: String): Resource[IO, BufferedSource] =
    Resource.make {
      IO(Source.fromResource(path))
    } { source =>
      IO(source.close()).handleErrorWith(_ => IO.unit)
    }

  def asInput(source: BufferedSource): String = source.getLines().mkString("\n")

}
