package lab.skaaj.utils

import zio.ZIO

extension [R, E, A](zio: ZIO[R, E, A])
  def debugThread: ZIO[R, E, A] =
    zio
      .tap(value => ZIO.succeed(println(s"[${Thread.currentThread().getName}] $value")))
      .tapErrorCause(cause => ZIO.succeed(println(s"[${Thread.currentThread().getName}][FAIL] $cause")))
