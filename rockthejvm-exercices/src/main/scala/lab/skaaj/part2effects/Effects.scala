package lab.skaaj.part2effects

import scala.io.StdIn

object Effects extends App {

  case class ToyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): ToyIO[B] =
      ToyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => ToyIO[B]): ToyIO[B] =
      ToyIO(() => f(unsafeRun()).unsafeRun())
  }

  /**
   * Exercises - create some IO which
   *  1. measure the current time of the system
   *     2. measure the duration of a computation
   *    - use exercise 1
   *    - use map/flatMap combinations of MyIO
   *      3. read something from the console
   *      4. print something to the console (e.g. "what's your name"), then read, then print a welcome message
   */

  def getCurrentTime: ToyIO[Long] = ToyIO(() => System.currentTimeMillis())

  def measureTime[A](computation: ToyIO[A]): ToyIO[(Long, A)] = for {
    startTime <- getCurrentTime
    result <- computation
    endTime <- getCurrentTime
  } yield (endTime - startTime, result)

  def readConsole: ToyIO[String] = ToyIO(() => StdIn.readLine())

  def writeConsole(text: String): ToyIO[Unit] = ToyIO(() => println(text))

  // Running all together
  val program = for {
    _ <- writeConsole("beginning")
    result <- measureTime(readConsole)
    _ <- writeConsole(s"you entered '${result._2}' in ${result._1} ms")
  } yield ()

  program.unsafeRun()
}
