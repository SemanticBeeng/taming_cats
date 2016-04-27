package di_using_monadic_composition

/** Functor */
trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

/** Monad */
trait Monad[M[_]] extends Functor[M] {
  def flatMap[A, B](a: M[A])(f: A => M[B]): M[B]
  def pure[A](a : A): M[A]
}

/** Identity monad */
case class Id[A](value: A) {
  def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] =
    f(a.value)
  def pure[A, B](a: A): Id[A] = Id(a)
}
