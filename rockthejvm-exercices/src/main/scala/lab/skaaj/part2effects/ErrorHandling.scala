package lab.skaaj.part2effects

import zio.*

import java.io.IOException
import scala.util.Try

object ErrorHandling extends ZIOAppDefault {

  /**
   * Exercices
   */

  // 0 - implement a version of fromTry, fromOption, fromEither, either, absolve
  //     using fold and foldZIO
  def fromTry[A](x: Try[A]): Task[A] =
    x.fold(ZIO.fail(_), ZIO.succeed(_))

  def fromOption[A](x: Option[A]): IO[Option[Nothing], A] =
    x match {
      case Some(v) => ZIO.succeed(v)
      case None => ZIO.fail(None)
    }

  def fromEither[E, A](x: Either[E, A]): IO[E, A] =
    x.fold(ZIO.fail(_), ZIO.succeed(_))

  def either[E, A](zio: IO[E, A]): UIO[Either[E, A]] =
    zio.foldZIO(
      error => ZIO.succeed(Left(error)),
      value => ZIO.succeed(Right(value))
    )

  def absolve[E, A](zio: IO[Nothing, Either[E, A]]): IO[E, A] =
    zio.flatMap {
      case Left(error) => ZIO.fail(error)
      case Right(value) => ZIO.succeed(value)
    }

  // 1 - make this effect fail with a typed error
  private val aBadFailure: UIO[Int] =
    ZIO.succeed[Int](throw new RuntimeException("oops"))

  private val aBetterFailure: IO[RuntimeException, Int] =
    aBadFailure.unrefine { case e: RuntimeException => e }

  // 2 - transform a zio into another zio with a narrower exception type
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineOrDie {
      case e: IOException => e
    }

  // 3
  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
    zio.right.mapError(_.swap)

  // 4
  private val database = Map(
    "daniel" -> 123,
    "alice" -> 789
  )

  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): IO[QueryError, Option[UserProfile]] =
    if (userId != userId.toLowerCase()) {
      ZIO.fail(QueryError("user ID format is invalid"))
    } else {
      ZIO.succeed(database.get(userId).map(phone => UserProfile(userId, phone)))
    }

  // surface out all the failed cases of this API
  def betterLookupProfile(userId: String): IO[Option[QueryError], UserProfile] =
    if (userId != userId.toLowerCase()) {
      ZIO.fail(Some(QueryError("user ID format is invalid")))
    } else {
      database.get(userId) match {
        case Some(phone) => ZIO.succeed(UserProfile(userId, phone))
        case None => ZIO.fail(None)
      }
    }

  def run = for {
    res <- aBetterFailure
    _ <- ZIO.succeed(println(res))
  } yield ()
}
