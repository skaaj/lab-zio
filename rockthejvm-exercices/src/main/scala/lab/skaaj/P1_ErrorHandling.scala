package lab.skaaj

import zio._

object P1_ErrorHandling extends ZIOAppDefault {

  /**
   * Exercices
   */

  // 1 - make this effect fail with a typed error
  val aBadFailure = ZIO.succeed[Int](throw new RuntimeException("oops"))
  val aBetterFailure = aBadFailure
    .catchSomeDefect {
      case e: RuntimeException => ZIO.fail(e)
    }
  val anotherBetterFailure = aBadFailure.unrefine {
    case e: RuntimeException => e
  }

  def run = anotherBetterFailure
}
