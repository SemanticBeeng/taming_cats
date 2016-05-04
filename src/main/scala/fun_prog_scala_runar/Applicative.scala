package fun_prog_scala_runar

import scala.language.higherKinds

/** Minimal combinators:
  * - map2, unit
  * - apply, unit
  */
trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]

  def apply[A, B](fa: F[A])(ff: F[A => B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    apply(fa)(unit(f))

  def map2[A , B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    apply(fa)(map(fb)(b => f(_, b)))

}
