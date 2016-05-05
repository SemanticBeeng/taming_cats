import simulacrum._
import scala.language.higherKinds

/*
 * Based on awesome video serie:
 * Functional Structures in Scala by Michael Pilquist
 * FSiS Part 3 - Monad type class
 *   https://www.youtube.com/watch?v=VWCtLhH815M
 */

/**
  * Minimal set of combinators for Monad:
  *  - unit, flatMap
  *  - unit, compose
  *  - unit, join, map
  *  - unit, join, apply
  *  - unit, join, map2
  */

@typeclass
trait Monad[Effect[_]] extends Applicative[Effect] { self =>

  def pure[A](value: A): Effect[A]

  def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B]

  override def apply[A, B](fa: Effect[A])(ff: Effect[A => B]): Effect[B] = {
    val helper: (A => B) => Effect[B] =
      (f: A => B) => map(fa)(f)

    flatMap(ff)(helper)
  }

  override def map[A, Z](fa: Effect[A])(f: A => Z): Effect[Z] =
    flatMap(fa)(a => pure(f(a)))

  def compose[Other[_]](implicit OtherApplicative: Monad[Other]):
    Monad[Lambda[OtherEffect => Effect[Other[OtherEffect]]]] =
    ??? // cant be done for any monad, yet can be done for Option, List,

  
  // derived methods

  def flatten[A](ffa: Effect[Effect[A]]): Effect[A] =
    flatMap(ffa)(identity)

}

object Monad {

  implicit val listMonad = new Monad[List] {

    def pure[A](value: A): List[A] = List(value)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa flatMap f
  }

  implicit val optionMonad = new Monad[Option] {
    def pure[A](value: A): Option[A] = Option(value)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa flatMap f
  }
}

trait MonadLaws[Effect[_]] {

  import Monad.ops._

  implicit val Tested: Monad[Effect]

  def flatMapAssociavitiy[A, B, C](fa: Effect[A], f: A => Effect[B], g: B => Effect[C]): Boolean =
    fa.flatMap(f).flatMap(g) == fa.flatMap { a => f(a).flatMap( b => g(b) ) }

  def leftIdentity[A, B](a: A, f: A => Effect[B]): Boolean =
    Tested.pure(a).flatMap(f) == f(a)

  def rightIdentity[A](fa: Effect[A]): Boolean =
    fa.flatMap{ a => Tested.pure(a) } == fa

}

object MonadLaws {
  def apply[Effect[_]](implicit monad:Monad[Effect]): MonadLaws[Effect] =
    new MonadLaws[Effect] {
      val Tested = monad
    }
}
