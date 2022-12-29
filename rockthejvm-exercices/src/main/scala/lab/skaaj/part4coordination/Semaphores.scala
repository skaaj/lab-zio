package lab.skaaj.part4coordination

import lab.skaaj.utils.*
import zio.*

import java.util.concurrent.TimeUnit

object Semaphores extends ZIOAppDefault {

  def doWorkWhileLoggedIn(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  /**
   * Exercises
   */
  val mySemaphore = Semaphore.make(1)
  val tasks = ZIO.collectAllPar((1 to 10).map { id =>
    for {
      sem <- mySemaphore
      _ <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
      res <- sem.withPermit {
        for {
          // critical section start
          _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
          res <- doWorkWhileLoggedIn()
          _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
        } yield res
      }
    } yield res
  })

  val tasksFixed = mySemaphore.flatMap { sem =>
    ZIO.collectAllPar((1 to 10).map { id =>
      for {
        _ <- ZIO.succeed(s"[task $id] waiting to log in").debugThread
        res <- sem.withPermit {
          for {
            // critical section start
            _ <- ZIO.succeed(s"[task $id] logged in, working...").debugThread
            res <- doWorkWhileLoggedIn()
            _ <- ZIO.succeed(s"[task $id] done: $res").debugThread
          } yield res
        }
      } yield res
    })
  }

  def run = tasksFixed.debugThread
}
