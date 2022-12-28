package lab.skaaj.part3concurrency

import zio.*

object Parallelism extends ZIOAppDefault {

  /**
   * Exercices
   */

  def countWords(str: String): ZIO[Any, Nothing, Int] =
    ZIO.succeed(str.split(" ").length)

  def countWordsParallel(files: Seq[String]): UIO[Int] = {
    ZIO.mergeAllPar(files.map(countWords))(0)(_ + _)
  }

  def run = for {
    res <- countWordsParallel((0 until 1_000_000).map(_ => "one two three"))
    _ <- ZIO.succeed(println(res))
  } yield ()
}
