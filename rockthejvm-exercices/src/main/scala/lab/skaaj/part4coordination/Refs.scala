package lab.skaaj.part4coordination

import lab.skaaj.utils.*
import zio.*

import java.util.concurrent.TimeUnit

object Refs extends ZIOAppDefault {

  /**
   * Exercises
   */

  // 1
  def tickingClockImpure(): Unit = {
    var ticks = 0L
    // print current time every 1s + increase a counter (ticks)
    def tickingClock: UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ZIO.succeed(ticks += 1)
      _ <- tickingClock
    } yield ()

    // print the total ticks count every 5s
    def printTicks: UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      _ <- ZIO.succeed(s"TICKS: $ticks").debugThread
      _ <- printTicks
    } yield ()

    (tickingClock zipPar printTicks).unit
  }

  def tickingClockPure(): UIO[Unit] = {
    // print current time every 1s + increase a counter (ticks)
    def tickingClock(ticks: Ref[Long]): UIO[Unit] = for {
      _ <- ZIO.sleep(1.second)
      _ <- Clock.currentTime(TimeUnit.MILLISECONDS).debugThread
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    // print the total ticks count every 5s
    def printTicks(ticks: Ref[Long]): UIO[Unit] = for {
      _ <- ZIO.sleep(5.seconds)
      currentTick <- ticks.get
      _ <- ZIO.succeed(s"TICKS: $currentTick").debugThread
      _ <- printTicks(ticks)
    } yield ()

    for {
      ticks <- Ref.make(0L)
      _ <- tickingClock(ticks) zipPar printTicks(ticks)
    } yield ()
  }

  def run = tickingClockPure()
}
