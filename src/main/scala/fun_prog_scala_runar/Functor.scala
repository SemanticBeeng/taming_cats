package fun_prog_scala_runar

import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

  def distribute[A, B](fab: F[(A,B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def unzip[A,B] = distribute[A,B] _
}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.map(f)
  }

  val optionFunctor = new Functor[Option] {
    def map[A, B](option: Option[A])(f: (A) => B): Option[B] =
      option.map(f)
  }
}
