package lab.skaaj.part4coordination

import lab.skaaj.utils.*
import zio.*

import java.util.concurrent.TimeUnit

object Promises extends ZIOAppDefault {

  /**
   * Exercises
   */

  // 1 - write a simulated "egg boiler" with two ZIOs
  // * one increments a counter every 1s
  // * one waits for the counter to become 10, after which it will "ring a bell"
  def eggBoiler(): Task[Unit] = {
    def countingTicks(ticks: Ref[Int], promise: Promise[Throwable, Int]): Task[Unit] = {
      for {
        _ <- ZIO.sleep(1.second)
        newTick <- ticks.updateAndGet(_ + 1)
        _ <- {
          if(newTick >= 10) promise.succeed(newTick)
          else countingTicks(ticks, promise)
        }
      } yield ()
    }

    def ringBell(promise: Promise[Throwable, Int]): Task[Unit] = {
      for {
        _ <- ZIO.succeed("waiting...").debugThread
        _ <- promise.await
        _ <- ZIO.succeed("egg boiled.").debugThread
      } yield ()
    }

    for {
      ticks <- Ref.make(0)
      promise <- Promise.make[Throwable, Int]
      _ <- countingTicks(ticks, promise) zipPar ringBell(promise)
    } yield ()
  }

  // 2 - write a "race pair"
  // * use a Promise which can hold and Either[exit for A, exit for B]
  // * start fiber for each ZIO
  // * on completion, each ZIO to complete that Promise (hint: use a finalizer)
  // * waiting on the Promise's value can be interrupted!
  // * if the whole race is interrupted, interrupt the running fibers

  def racePair[R, E, A, B](
    zioa: => ZIO[R, E, A],
    ziob: => ZIO[R, E, B]
  ): ZIO[R, Nothing, Either[(Exit[E, A], Fiber[E, B]), (Fiber[E, A], Exit[E, B])]] =
  ZIO.uninterruptibleMask { restore =>
    for {
      promise <- Promise.make[Nothing, Either[Exit[E, A], Exit[E, B]]]
      fiba <- zioa.onExit(exita => promise.succeed(Left(exita))).fork
      fibb <- ziob.onExit(exitb => promise.succeed(Right(exitb))).fork
      result <- restore(promise.await).onInterrupt {
        for {
          fibaInterruptFib <- fiba.interrupt.fork
          fibbInterruptFib <- fibb.interrupt.fork
          _ <- fibaInterruptFib.join
          _ <- fibbInterruptFib.join
        } yield ()
      }
    } yield result match {
      case Left(exita) => Left((exita, fibb))
      case Right(exitb) => Right((fiba, exitb))
    }
  }

  val demoRacePair = {
    val zioa = ZIO.sleep(1.second).as(1).onInterrupt(ZIO.succeed("first interrupted").debugThread)
    val ziob = ZIO.sleep(2.second).as(2).onInterrupt(ZIO.succeed("second interrupted").debugThread)
    val pair = racePair(zioa, ziob)
    pair.flatMap {
      case Left((exita, fibb)) => fibb.interrupt *> ZIO.succeed("first won").debugThread *> ZIO.succeed(exita).debugThread
      case Right((fiba, exitb)) => fiba.interrupt *> ZIO.succeed("second won").debugThread *> ZIO.succeed(exitb).debugThread
    }
  }

  def run = demoRacePair
}
