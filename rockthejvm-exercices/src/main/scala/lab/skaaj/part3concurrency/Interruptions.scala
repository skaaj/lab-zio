package lab.skaaj.part3concurrency

import zio.*

object Interruptions extends ZIOAppDefault {

  /**
   * Exercices
   */

  def timeout[R, E, A](zio: ZIO[R, E, A], duration: Duration): ZIO[R, E, A] = {
    for {
      fib <- zio.fork
      _ <- (ZIO.sleep(duration) *> fib.interrupt).fork
      res <- fib.join
    } yield res
  }

  def timeout_v2[R, E, A](zio: ZIO[R, E, A], duration: Duration): ZIO[R, E, Option[A]] = {
    timeout(zio, duration).foldCauseZIO(
      cause => if(cause.isInterrupted) ZIO.succeed(None) else ZIO.failCause(cause),
      value => ZIO.succeed(Some(value))
    )
  }


  def run = for {
    _ <- ZIO.succeed("OOF")
  } yield ()
}
