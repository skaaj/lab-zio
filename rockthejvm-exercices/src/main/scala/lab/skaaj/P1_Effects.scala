package lab.skaaj

import zio._

import scala.io.StdIn

object P1_Effects extends ZIOAppDefault {

  /**
   * Exercices
   */

  // 1
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      va <- zioa
      vb <- ziob
    } yield vb

  // 2
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    for {
      va <- zioa
      vb <- ziob
    } yield va

  // 3
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.flatMap(_ => runForever(zio))

  val endlessLoop = runForever {
    ZIO.succeed {
      println("running...")
      //Thread.sleep(1000)
    }
  }

  // 4
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    for {
      _ <- zio
    } yield value

  // 5
  def discard[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] = convert(zio, ())

  // 6
  def sum(n: Int): Int =
    if n == 0 then 0 else n + sum(n - 1)

  def sumZIO(n: Int): UIO[Int] =
    if n == 0 then ZIO.succeed(0)
    else for {
      current <- ZIO.succeed(n)
      next <- sumZIO(n - 1)
    } yield current + next

  // 7
  // hint: use ZIO.suspend/ZIO.suspendSucceed
  def fiboZIO(n: Int): UIO[BigInt] =
    if n <= 1 then ZIO.succeed(n)
    else for {
      _ <- ZIO.succeed(314)
      n_1 <- fiboZIO(n - 1)
      n_2 <- fiboZIO(n - 2)
    } yield n_1 + n_2

  def run = fiboZIO(6).flatMap(x => ZIO.succeed(println(x)))
}
