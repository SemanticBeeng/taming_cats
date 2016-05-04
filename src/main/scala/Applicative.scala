/*
 * Based on awesome video serie: Functional Structures in Scala
 * by: Michael Pilquist
 * part: FSiS Part 2 - Applicative type class
 * https://www.youtube.com/watch?v=tD_EyIKqqCk
 */

import simulacrum._
import scala.language.higherKinds

/**
  * Applicative functor.
  *
  * map apply function in context (effect)
  * pure put in context (effect)
  *
  * More stuff:
  *  The Essence of the Iterator Pattern: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
  *  Applicative programming with effects: http://staff.city.ac.uk/~ross/papers/Applicative.pdf
  *  Move Over Free Monads: Make Way for Free Applicatives! — John de Goes: https://www.youtube.com/watch?v=H28QqxO7Ihc
  *
  * Effect might not be side effect
  * Examples of effects:
  *   Option - effect of having a value or not having a value
  *   LIst - effect of having multiple values
  */
@typeclass trait Applicative[Effect[_]] extends Functor[Effect] {  self =>

  /** Take value and put (lift) into effect / container  */
  def pure[A](value: A): Effect[A]

  // TODO scaladoc with interpreteation in terms of effect
  def apply[A, B](fa: Effect[A])(ff: Effect[A => B]): Effect[B]

  def apply2[A, B, Z](fa: Effect[A], fb: Effect[B])(ff: Effect[(A,B) => Z]): Effect[Z] =
    apply(fa)(apply(fb)(map(ff)(f => b => a => f(a,b)))) // TODO understand/rename/extract variables it

  override def map[A, B](contA: Effect[A])(funAtoB: A => B): Effect[B] =
    apply(contA)(pure(funAtoB)) // TODO how it works

  /** for 2 values with effect apply f on values to get result with effect */
  def map2[A , B, Z](fa: Effect[A], fb: Effect[B])(f: (A, B) => Z): Effect[Z] =
    apply(fa)(map(fb)(b => f(_, b))) // TODO understand/rename/extract variables it

  def map3[A, B, C, Z](fa: Effect[A], fb: Effect[B], fc: Effect[C])(f: (A, B, C) => Z): Effect[Z] =
    apply(fa)(map2(fb, fc)((b, c) => a => f(a, b, c))) // TODO understand/rename/extract variables it

  def map4[A, B, C, D, Z](fa: Effect[A], fb: Effect[B], fc: Effect[C], fd: Effect[D])(f: (A, B, C, D) => Z): Effect[Z] =
    map2(tuple2(fa, fb), tuple2(fc, fd)) { case ((a, b), (c, d)) => f(a, b, c, d) }
    // this implementation works too: apply(fa)(map3(fb, fc, fd)((b, c, d) => a => f(a, b, c, d)))

  def tuple2[A, B](fa: Effect[A], fb: Effect[B]): Effect[(A, B)] =
    map2(fa, fb)((a, b) => (a,b)) // TODO understand/rename/extract variables it

  def tuple3[A, B, C](fa: Effect[A], fb: Effect[B], fc: Effect[C]): Effect[(A, B, C)] =
    map3(fa, fb, fc)((a, b, c) => (a, b, c)) // TODO understand/rename/extract variables it

  // you can continue untill 22

  def flip[A, B](ff: Effect[A => B]): Effect[A] => Effect[B] =
    fa => apply(fa)(ff)

  def compose[OtherApplicative[_]](implicit OtherApplicative: Applicative[OtherApplicative]):
        Applicative[Lambda[OtherEffect => Effect[OtherApplicative[OtherEffect]]]] =
    new Applicative[Lambda[OtherEffect => Effect[OtherApplicative[OtherEffect]]]] {

      override def pure[A](value: A): Effect[OtherApplicative[A]] =
        self.pure(OtherApplicative.pure(value))

      override def apply[A, B](fga: Effect[OtherApplicative[A]])(ff: Effect[OtherApplicative[(A) => B]]):
        Effect[OtherApplicative[B]] = {
          val x: Effect[OtherApplicative[A] => OtherApplicative[B]] = self.map(ff)(gab => OtherApplicative.flip(gab))
          self.apply(fga)(x)
      }
    }
}

object Applicative {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](value: A): Option[A] = Some(value) // TODO maybe it should be Option(value)
    override def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
      case(None, _) => None
      case(Some(a), None) => None
      case(Some(a), Some(f)) => Some(f(a)) // TODO maybe Option(f(a))
    }
  }

  implicit val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](value: A): List[A] = List(value)

    /* First implementation:
      (fa zip ff).map { case(a,f) => f(a) }
    scala> Applicative[List].map(List(1, 2, 3))(_ + 1)
    res4: List[Int] = List(2)
    */
    override def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] = for {
        a <- fa
        f <- ff
      } yield f(a)
  }

  implicit val streamApplicative: Applicative[Stream] = new Applicative[Stream] {

    override def pure[A](value: A): Stream[A] = ???

    override def apply[A, B](fa: Stream[A])(ff: Stream[(A) => B]): Stream[B] = ???
  }
}


trait ApplicativeLaws[F[_]] { // TODO rename types to reflect Effect etc

  import Applicative.ops._

  implicit def F: Applicative[F]

  def applicativeIdentity[A](fa: F[A]) =
    fa.apply(F.pure((a: A) => a)) == fa // TODO understand

  def applicativeHomomorphism[A, B](a: A, f: A => B) =
    F.pure(a).apply(F.pure(f)) == F.pure(f(a)) // TODO understand

//  def applicativeInterchange[A, B](a: A, ff: F[A => B]) = // TODO dont work :(
//    F.pure(a).apply(ff) == ff.apply(F.pure(f => f(a))) // TODO UNDERSTAND mind bending stuff in here

  def applicativeMap[A, B](fa: F[A], f: A => B) =
    fa.map(f) == fa.apply(F.pure(f))
}

