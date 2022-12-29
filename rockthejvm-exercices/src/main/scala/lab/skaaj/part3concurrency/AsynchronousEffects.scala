package lab.skaaj.part3concurrency

import lab.skaaj.utils.*
import zio.*

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object AsynchronousEffects extends ZIOAppDefault {

  /**
   * Exercises
   */

  // 1 - lift a computation running on some (external) thread to a ZIO
  def external2ZIO[A](computation: () => A)(executor: ExecutorService): Task[A] = {
    ZIO.async { cb =>
      executor.execute { () =>
        try {
          val result = computation()
          cb(ZIO.succeed(result))
        } catch {
          case t: Throwable =>
            cb(ZIO.fail(t))
        }
      }
    }
  }

  val demoExternal2ZIO: Task[Unit] = {
    val executor = Executors.newFixedThreadPool(8)
    val zio: Task[Int] = external2ZIO { () =>
      println(s"[${Thread.currentThread().getName}] computing the meaning of life on some thread")
      Thread.sleep(1000)
      42
    }(executor)

    zio.debugThread.unit
  }

  // 2 - lift a Future to a ZIO
  def future2ZIO[A](future: => Future[A])(implicit ec: ExecutionContext): Task[A] = {
    ZIO.async { cb =>
      future.onComplete {
        case Success(value) => cb(ZIO.succeed(value))
        case Failure(exception) => cb(ZIO.fail(exception))
      }
    }
  }

  val demoFuture2ZIO: Task[Unit] = {
    val executor = Executors.newFixedThreadPool(8)
    given ExecutionContext = ExecutionContext.fromExecutorService(executor)
    val mol = future2ZIO(Future {
      println(s"[${Thread.currentThread().getName}] computing the meaning of life on some thread (future)")
      Thread.sleep(1000)
      42
    })

    mol.debugThread.unit
  }

  // 3 - implement a never ending ZIO
  def neverEndingZIO[A]: UIO[A] = ZIO.async[Any, Nothing, A](_ => ())

  def run = demoFuture2ZIO
}
