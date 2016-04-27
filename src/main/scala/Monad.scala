import simulacrum._
import scala.language.higherKinds

@typeclass trait Monad[Effect[_]] extends Applicative[Effect] { self =>

  // TODO already in Applicative
  def pure[A](value: A): Effect[A]

  def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B]

  override def apply[A, B](fa: Effect[A])(ff: Effect[A => B]): Effect[B] =
    flatMap(ff)(f => map(fa)(f))

  // derived methods



}
