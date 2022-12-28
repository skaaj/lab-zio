package lab.skaaj.part3concurrency

import zio.*

object Fibers extends ZIOAppDefault {

  /**
   * Exercices
   */

  def zipFibers[E, A, B](fiber1: Fiber[E, A], fiber2: Fiber[E, B]): ZIO[Any, Nothing, Fiber[E, (A, B)]] = {
    (for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)).fork
  }

  def chainFibers[E, A](fiber1: Fiber[E, A], fiber2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] = {
    fiber1.join.orElse(fiber2.join).fork
  }

  def countWords(str: String): ZIO[Any, Nothing, Int] =
    ZIO.succeed(str.split(" ").length)
  def countWordsParallel(files: Seq[String]): UIO[Int] = {
    files.map(countWords(_).fork.flatMap(_.join)).foldLeft(ZIO.succeed[Int](0)) {
      case (acc, item) => for {
        prev <- acc
        current <- item
      } yield prev + current
    }
  }

  def run = for {
    res <- countWordsParallel((0 until 5).map(_ => "one two three"))
    _ <- ZIO.succeed(println(res))
  } yield ()
}
