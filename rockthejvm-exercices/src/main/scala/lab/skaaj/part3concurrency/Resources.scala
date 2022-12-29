package lab.skaaj.part3concurrency

import zio.*

import java.io.File
import java.util.Scanner

object Resources extends ZIOAppDefault {

  /**
   * Exercises
   */

  def openFileScanner(path: String): UIO[Scanner] = {
    ZIO.succeed(new Scanner(new File(path)))
  }

  def acquireOpenFile(path: String): UIO[Unit] = {
    ZIO.acquireReleaseWith(openFileScanner(path))(s => ZIO.succeed(s.close())) { scanner =>
      ZIO.iterate(scanner)(_.hasNextLine())(s =>
        ZIO.succeed(println(s.nextLine()))
          *> ZIO.sleep(100.millis)
          *> ZIO.succeed(s)
      ).map(_ => ())
    }
  }

  private val testInterruptFileDisplay = for {
    fib <- acquireOpenFile("src/main/scala/lab/skaaj/part3concurrency/Resources.scala").fork
    _ <- ZIO.sleep(2.seconds) *> fib.interrupt
  } yield ()

  def run = testInterruptFileDisplay
}
