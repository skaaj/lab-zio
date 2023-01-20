package controllers

import javax.inject._
import play.api.mvc._
import zio._
import core.Extensions._

@Singleton
class MainController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
  private implicit val zioRuntime = Runtime.default

  def index = Action.asyncZIO { request =>
    val process = for {
      randomInt <- Random.nextInt
      _ <- Console.printLine(s"received: $request")
      _ <- Console.printLine(s"generated: $randomInt")
    } yield Ok(randomInt.toString)

    process.repeatN(5)
  }
}
