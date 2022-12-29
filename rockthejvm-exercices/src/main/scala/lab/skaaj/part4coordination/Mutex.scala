package lab.skaaj.part4coordination

import lab.skaaj.utils.*
import zio.*

import java.util.concurrent.TimeUnit
import scala.collection.immutable.Queue

abstract class Mutex {
  def acquire: UIO[Unit]
  def release: UIO[Unit]
}

object Mutex {
  type Signal = Promise[Nothing, Unit]
  case class State(locked: Boolean, waiting: Queue[Signal])
  def make: UIO[Mutex] = Ref.make(State(false, Queue())).map { stateRef =>
    new Mutex {
      override def acquire: UIO[Unit] = for {
        promise <- Promise.make[Nothing, Unit]
        shouldWait <- stateRef.modify {
          case State(true, waiting) => (true, State(true, waiting.enqueue(promise)))
          case State(false, _) => (false, State(true, Queue()))
        }
        _ <- if(shouldWait) promise.await else ZIO.unit
      } yield ()

      override def release: UIO[Unit] = for {
        promiseOpt <- stateRef.modify {
          case State(_, promise +: remaining) =>
            (Some(promise), State(remaining.nonEmpty, remaining))
          case State(_, _) =>
            (None, State(false, Queue()))
          }
        _ <- promiseOpt.fold(ZIO.unit)(_.succeed(()))
      } yield ()
    }
  }
}

object MutexPlayground extends ZIOAppDefault {

  def workInCriticalRegion(): UIO[Int] =
    ZIO.sleep(1.second) *> Random.nextIntBounded(100)

  def demoNonLockingTasks() =
    ZIO.collectAllParDiscard((0 until 10).toList.map { i =>
      for {
        _ <- ZIO.succeed(s"[task $i] working...").debugThread
        result <- workInCriticalRegion()
        _ <- ZIO.succeed(s"[task $i] got result: $result").debugThread
      } yield ()
    })

  def createTask(id: Int, mutex: Mutex): UIO[Int] = for {
    _ <- ZIO.succeed(s"[task $id] waiting for mutex...").debugThread
    _ <- mutex.acquire
    _ <- ZIO.succeed(s"[task $id] mutex acquired, working...").debugThread
    result <- workInCriticalRegion()
    _ <- ZIO.succeed(s"[task $id] got result: $result, releasing mutex").debugThread
    _ <- mutex.release
  } yield result

  def demoLockingTasks = for {
    mutex <- Mutex.make
    _ <- ZIO.collectAllParDiscard((0 until 10).toList.map(i => createTask(i, mutex)))
  } yield ()

  def run = demoLockingTasks
}
