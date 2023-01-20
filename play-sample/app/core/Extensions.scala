package core

import zio._
import play.api.mvc._

object Extensions {
  implicit class ZIOExt[R, E, A](zio: ZIO[R, E, A]) {
    def debugThread: ZIO[R, E, A] = {
      zio
        .tap(value => ZIO.succeed(println(s"[${Thread.currentThread().getName}] $value")))
        .tapErrorCause(cause => ZIO.succeed(println(s"[${Thread.currentThread().getName}][FAIL] $cause")))
    }
  }

  implicit class ActionExt[A](actionBuilder: ActionBuilder[Request, A]) {
    def asyncZIO(zioAction: Request[A] => Task[Result])(implicit runtime: Runtime[Any]): Action[A] =
      actionBuilder.async { request =>
        Unsafe.unsafe { implicit unsafe =>
          runtime.unsafe.runToFuture(zioAction(request))
        }
      }
  }
}
