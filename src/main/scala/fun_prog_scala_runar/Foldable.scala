package fun_prog_scala_runar

import scala.language.higherKinds

trait Foldable[F[_]] {
  def foldLeft[A,B](as: F[A])(init: B)(f: (B,A) => B): B

  def foldRight[A, B](as: F[A])(init: B)(f: (A,B) => B): B =
    foldLeft(as)(init)((acc: B, a: A) => f(a,acc) )

  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)( (acc: B, a: A) => mb.op(acc, f(a)) )

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](as: F[A]): List[A] =
    foldLeft(as)(List[A]())( (acc: List[A], b:A) => b :: acc )
}

object ListFoldable extends Foldable[List] {
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    as.foldLeft(z)(f)
}

object OptionFoldable extends Foldable[Option] {
  def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    as.map( f(z, _) ).getOrElse(z)
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] { // TODO
  def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = ???
}

object StreamFoldable extends Foldable[Stream] { // TODO stream
  def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = ???
}