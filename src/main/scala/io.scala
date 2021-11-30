import cats.{Applicative, ApplicativeError}
import cats.syntax.all._
import cats.effect.kernel.Resource

import scala.io.{BufferedSource, Source}

object io {

  //TODO abstract over F instead of having an explicit IO here
  def load[F[_]](path: String)(using AE:ApplicativeError[F, Throwable]): Resource[F, BufferedSource] =
    Resource.make {
      Applicative[F].pure(Source.fromResource(path))
    }{ source =>
      Applicative[F].pure(source.close()).handleErrorWith(_ => Applicative[F].unit)
    }



}
