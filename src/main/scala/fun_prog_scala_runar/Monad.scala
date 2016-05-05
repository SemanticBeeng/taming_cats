package fun_prog_scala_runar

import scala.language.higherKinds

/** Minimal set of combinators:
  *  - unit, flatMap
  *  - unit, compose
  *  - unit, join, map
  *  - unit, join, apply
  *  - unit, join, map2
  */
trait Monad[M[_]] extends Applicative[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

  override def map[A, B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)( (a:A) => unit( f(a) ) )

  override def map2[A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a,b)))

//  def apply[A, B](ff: M[A => B])(fa: M[A]): M[B] = ???
//    flatMap(ff)((f:B) => flatMap(fa)(a => f(a)))



//  def apply[A, B](fa: List[A])(ff: List[(A) => B]): List[B] = ???

  override def apply[A, B](fa: M[A])(ff: M[(A) => B]): M[B] = ???

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    traverse(lma)(ma => ma)

  def traverse[A, B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight( unit(List[B]()) )( (a, mbs) => map2(f(a), mbs)(_ :: _))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    map(ma)( (a:A) => List.fill(n)(a) )

  def factor[A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    map2(ma, mb)((_, _))

  def cofactor[A, B](e: Either[M[A], M[B]]): M[Either[A, B]] = ??? // TODO

  // TODO Kleisli arrow A => M[B] can be composed:
  def compose[A, B, C](f: A => M[B], g: B => M[C]): A => M[C] = ??? // TODO

  def join[A](mma: M[M[A]]): M[A] = ??? // TODO

}

object Monad {

  val listMonad = new Monad[List] {

    def unit[A](a: => A): List[A] =
      List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] =
      Option(a)

    def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad = new Monad[Stream] { // TODO stream
    def unit[A](a: => A): Stream[A] = ???

    def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] = ???
  }
}


