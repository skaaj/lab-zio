package lab.skaaj

import zio.{ZIO, ZIOAppDefault}

object P2_ErrorHandling extends ZIOAppDefault {

  /**
   * Exercices
   */

  // 1 - make this effect fail with a typed error
  val aBadFailure = ZIO.succeed[Int](throw new RuntimeException("oops"))

  def run = aBadFailure
}
