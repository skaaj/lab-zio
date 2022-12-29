package lab.skaaj.part3concurrency

import zio.*
import lab.skaaj.utils.*

object MasteringInterruptions extends ZIOAppDefault {

  private val inputPassword = for {
    _ <- ZIO.succeed("Input password:").debugThread
    _ <- ZIO.succeed("(typing password)").debugThread
    _ <- ZIO.sleep(4.seconds)
    pass <- ZIO.succeed("123")
  } yield pass

  private def verifyPassword(password: String) = for {
    _ <- ZIO.succeed("verifying...").debugThread
    _ <- ZIO.sleep(2.seconds)
    result <- ZIO.succeed(password == "123")
  } yield result

  val authFlow = ZIO.uninterruptibleMask { restore =>
    for {
      password <- restore(inputPassword).onInterrupt(ZIO.succeed("Authentication timed out.").debugThread)
      verification <- verifyPassword(password)
      _ <- {
        if(verification) ZIO.succeed("Authentication succeeded.").debugThread
        else ZIO.succeed("Authentication failed.").debugThread
      }
    } yield ()
  }

  /**
   * Exercises
   */

  // 1
  val cancelBeforeMol = ZIO.interrupt *> ZIO.succeed(42).debugThread
  val uncancelBeforeMol = ZIO.interruptible(ZIO.interrupt *> ZIO.succeed(42).debugThread)

  // 2
  val authProgram = for {
    authFib <- ZIO.uninterruptibleMask (_ => authFlow).fork
    _ <- ZIO.sleep(3.seconds) *> ZIO.succeed("Attempting to cancel authentication...").debugThread *> authFib.interrupt
    _ <- authFib.join
  } yield ()

  // 3
  val threeStepProgram = {
    val sequence = ZIO.uninterruptibleMask { restore =>
      for {
        _ <- restore(ZIO.succeed("interruptible 1").debugThread *> ZIO.sleep(1.second))
        _ <- ZIO.succeed("uninterruptible").debugThread *> ZIO.sleep(1.second)
        _ <- restore(ZIO.succeed("interruptible 2").debugThread *> ZIO.sleep(1.second))
      } yield ()
    }

    for {
      fib <- sequence.fork
      _ <- ZIO.sleep(1500.millis) *> ZIO.succeed("INTERRUPTING!").debugThread *> fib.interrupt
      _ <- fib.join
    } yield ()
  }

  def run = threeStepProgram
}
