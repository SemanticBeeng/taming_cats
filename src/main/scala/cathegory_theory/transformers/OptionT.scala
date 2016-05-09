package cathegory_theory.transformers

import simulacrum._
import scala.language.higherKinds

// based on FSiS Part 7 - OptionT transformer

import cathegory_theory._

final case class OptionT[F[_], A](value: F[Option[A]]) {

  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] =
    OptionT(F.map(value)(opta => opta map f))

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)( opta =>
      opta.map {
        a => f(a).value }.getOrElse(F.pure(None))
    ))

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] = ???
//    flatMap(f andThen OptionT.apply) TODO not compiling

  def isDefined(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isDefined)

  def isEmpty(implicit F: Functor[F]): F[Boolean] = F.map(value)(_.isEmpty)

  def getOrElse(default: => A)(implicit F:Functor[F]): F[A] =
    F.map(value)( opta => opta.getOrElse(default))
}

// TODO 35 example for REPL
// TODO 52 more REPL examples

object OptionT {

  implicit def functorOptionT[F[_]](implicit F: Functor[F]): Functor[OptionT[F, ?]] =
    new Functor[OptionT[F, ?]] {
      def map[A, B](fa: OptionT[F, A])(f: A => B): OptionT[F, B] =
        fa.map(f)
    }

  implicit def monadOptionT[F[_]](implicit F: Monad[F]): Monad[OptionT[F, ?]] =
    new Monad[OptionT[F, ?]] {

      def pure[A](value: A): OptionT[F, A] =
        OptionT(F.pure(Some(value)))

      def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
        fa.flatMap(f)
  }

}
