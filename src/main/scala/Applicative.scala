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

  /** Take value (ouside effect) and put (lift) into effect / container  */
  def pure[A](value: A): Effect[A]

  /**
    * Apply function under effect for value in effect
    */
  def apply[A, B](fa: Effect[A])(ff: Effect[A => B]): Effect[B]

  def apply2[A, B, Z](fa: Effect[A], fb: Effect[B])(ff: Effect[(A,B) => Z]): Effect[Z] =
    apply(fa)(apply(fb)(map(ff)(f => b => a => f(a,b))))

  /** map a function from A to B  on value inside Effect
    * to produce Effect with result */
  override def map[A, Z](fa: Effect[A])(f: A => Z): Effect[Z] =
    apply(fa)(pure(f))

  /** for 2 values with effect apply f on values to get result with effect */
  def map2[A , B, Z](fa: Effect[A], fb: Effect[B])(f: (A, B) => Z): Effect[Z] =
    apply(fa)(map(fb)(b => a => f(a, b)))

  def map3[A, B, C, Z](fa: Effect[A], fb: Effect[B], fc: Effect[C])(f: (A, B, C) => Z): Effect[Z] =
    apply(fa)(map2(fb, fc)((b, c) => a => f(a, b, c)))

  def map4[A, B, C, D, Z](fa: Effect[A], fb: Effect[B], fc: Effect[C], fd: Effect[D])(f: (A, B, C, D) => Z): Effect[Z] =
    apply(fa)(map3(fb, fc, fd)((b, c, d) => a => f(a, b, c, d)))
   //map2(tuple2(fa, fb), tuple2(fc, fd)) { case ((a, b), (c, d)) => f(a, b, c, d) }

  def tuple2[A, B](fa: Effect[A], fb: Effect[B]): Effect[(A, B)] =
    map2(fa, fb)((a, b) => (a,b))

  def tuple3[A, B, C](fa: Effect[A], fb: Effect[B], fc: Effect[C]): Effect[(A, B, C)] =
    map3(fa, fb, fc)((a, b, c) => (a, b, c))

  def tuple4[A, B, C, D](fa: Effect[A], fb: Effect[B], fc: Effect[C], fd: Effect[D]): Effect[(A, B, C, D)] =
    map4(fa, fb, fc, fd)((a, b, c, d) => (a, b, c, d))

  // you can continue untill 22

  def flip[A, B](ff: Effect[A => B]): Effect[A] => Effect[B] =
    fa => apply(fa)(ff)

  def compose[Other[_]](implicit OtherApplicative: Applicative[Other]):
        Applicative[Lambda[OtherEffect => Effect[Other[OtherEffect]]]] =
    new Applicative[Lambda[OtherEffect => Effect[Other[OtherEffect]]]] {

      override def pure[A](value: A): Effect[Other[A]] =
        self.pure(OtherApplicative.pure(value))

      override def apply[A, B](fga: Effect[Other[A]])(ff: Effect[Other[A => B]]):
        Effect[Other[B]] = {
          val helper: Effect[Other[A] => Other[B]] =
            self.map(ff)(gab => OtherApplicative.flip(gab))

          self.apply(fga)(helper)
      }
    }
}

object Applicative {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](value: A): Option[A] = Option(value)

    override def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] = (fa, ff) match {
      case(None, _) => None
      case(Some(a), None) => None
      case(Some(a), Some(f)) => Some(f(a))
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

  // TODO stream applicative
  implicit val streamApplicative: Applicative[Stream] = new Applicative[Stream] {

    override def pure[A](value: A): Stream[A] = ???

    override def apply[A, B](fa: Stream[A])(ff: Stream[(A) => B]): Stream[B] = ???
  }

  // TODO Future applicative
}


trait ApplicativeLaws[Effect[_]] {

  import Applicative.ops._

  implicit def Tested: Applicative[Effect]

  /* like Functor identity but in terms of apply and pure */
  def applicativeIdentity[A](fa: Effect[A]) =
    fa.apply(Tested.pure((a:A) => a)) == fa

  /* apply lifetd function on lifted arg is like lifting result of applying f on a */
  def applicativeHomomorphism[A, B](a: A, f: A => B) =
    Tested.pure(a).apply(Tested.pure(f)) == Tested.pure(f(a))

  /* TODO 54.49 https://youtu.be/tD_EyIKqqCk?t=3289 */
  def applicativeInterchange[A, B](a: A, ff: Effect[A => B]) =
    Tested.pure(a).apply(ff) == ff.apply(Tested.pure((f: A => B) => f(a)))

  def applicativeMap[A, B](fa: Effect[A], f: A => B) =
    fa.map(f) == fa.apply(Tested.pure(f))
}

