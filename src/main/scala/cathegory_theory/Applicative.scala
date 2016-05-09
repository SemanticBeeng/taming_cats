package cathegory_theory

/*
 * Based on awesome video serie: Functional Structures in Scala
 * by: Michael Pilquist
 * part: FSiS Part 2 - Applicative type class
 * https://www.youtube.com/watch?v=tD_EyIKqqCk
 */

import simulacrum._
import scala.language.higherKinds

/**
  * Applicative functor
  *
  * It is a Functor but less expressive than Monad.
  *
  * Interpretation:
  * Encapsulates certain “effectful” computations in a functionally pure way
  *
  * map (from Functor) apply function inside context (effect)
  * adds 2 operations:
  *  - pure (unit) put value inside context (effect)
  *  - apply (app, <*>, splat) apply a function which is itself in a context to a value in a context
  *
  * Examples of effects:
  *   Option - effect of having a value or not having a value
  *   LIst - effect of having multiple values
  *
  * Minimal combinators:
  * - map2, unit
  * - apply, unit
  *
  * More stuff:
  *  The Essence of the Iterator Pattern: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
  *  Applicative programming with effects: http://staff.city.ac.uk/~ross/papers/Applicative.pdf
  *  Move Over Free Monads: Make Way for Free Applicatives! — John de Goes: https://www.youtube.com/watch?v=H28QqxO7Ihc
  *
  * Effect might not be side effect

  *
  * Described:
  * 2008, Conor McBride & Ross Paterson, Applicative Programming with Effects
  * TODO http://www.staff.city.ac.uk/~ross/papers/Applicative.html
  */
@typeclass
trait Applicative[Effect[_]] extends Functor[Effect] {  self =>

  /**
    * Take value (ouside effect) and put (lift) into effect / container
    *
    * Creates some sort of “default” container or “effect free” context.
    * Can be treated as constructor.
    *
    * Usually, for a given implementation of apply
    * there is only one possible implementation of pure.
    */
  def pure[A](value: A): Effect[A]

  /**
    * Apply function under effect for value in effect
    *
    * For given value in effect take transformation under effect
    * and apply it
    *
    * for List:  apply[A, B] (fa: List[A]) (ff: List[A => B]): List[B]
    *
    *            for list of A and list of functions from A to B
    *            apply for each value appropriate function
    *
    * for Option: apply[A, B] (fa: Option[A]) (ff: Option[A => B]): Option[B]
    *
    *             for optional value A and optional transformation A to B
    *             apply this transformation
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

      def pure[A](value: A): Effect[Other[A]] =
        self.pure(OtherApplicative.pure(value))

      def apply[A, B](fga: Effect[Other[A]])(ff: Effect[Other[A => B]]):
        Effect[Other[B]] = {
          val helper: Effect[Other[A] => Other[B]] =
            self.map(ff)(gab => OtherApplicative.flip(gab))

          self.apply(fga)(helper)
      }
    }
}

/**
  * Most of the standard types which are instances of Functor are also instances of Applicative
  */
object Applicative {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {

    def pure[A](value: A): Option[A] =
      Option(value)

    def apply[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] =
      (fa, ff) match {
        case(None, _) => None
        case(Some(a), None) => None
        case(Some(a), Some(f)) => Some(f(a))
      }
  }

  // TODO Haskell: http://www.springerlink.com/content/y7450255v2670167/
  implicit val listApplicative: Applicative[List] = new Applicative[List] {

    def pure[A](value: A): List[A] =
      List(value)

    def apply[A, B](fa: List[A])(ff: List[A => B]): List[B] = for {
        a <- fa
        f <- ff
      } yield f(a)
  }

  // TODO other list Applicative
  /*
  The list type constructor [] can actually be made an instance of Applicative in two ways; essentially, it comes down to whether we want to think of lists as ordered collections of elements, or as contexts representing multiple results of a nondeterministic computation (see Wadler’s How to replace failure by a list of successes).

Let’s first consider the collection point of view. Since there can only be one instance of a given type class for any particular type, one or both of the list instances of Applicative need to be defined for a newtype wrapper; as it happens, the nondeterministic computation instance is the default, and the collection instance is defined in terms of a newtype called ZipList. This instance is:

newtype ZipList a = ZipList { getZipList :: [a] }

instance Applicative ZipList where
  pure = undefined   -- exercise
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)
To apply a list of functions to a list of inputs with (<*>), we just match up the functions and inputs elementwise, and produce a list of the resulting outputs. In other words, we “zip” the lists together with function application, ($); hence the name ZipList.

The other Applicative instance for lists, based on the nondeterministic computation point of view, is:

instance Applicative [] where
  pure x    = [x]
  gs <*> xs = [ g x | g <- gs, x <- xs ]
Instead of applying functions to inputs pairwise, we apply each function to all the inputs in turn, and collect all the results in a list.

Now we can write nondeterministic computations in a natural style. To add the numbers 3 and 4 deterministically, we can of course write (+) 3 4. But suppose instead of 3 we have a nondeterministic computation that might result in 2, 3, or 4; then we can write

  pure (+) <*> [2,3,4] <*> pure 4
or, more idiomatically,

  (+) <$> [2,3,4] <*> pure 4.
   */

  implicit val infiniteStreamApplicative: Applicative[Stream] = new Applicative[Stream] {

    def pure[A](value: A): Stream[A] =
      Stream.continually(value)

    def apply[A, B](fa: Stream[A])(ff: Stream[A => B]): Stream[B] =
      (fa zip ff).map { case(a,f) => f(a) }
  }

  // TODO Future applicative

  // TODO Scala: Either applicative

  // TODO Scala: FunctionInt1

  // TODO Scala: Tuple with same types applicative

  /* TODO FPiS: IO Monad Applicative
  IO is an instance of Applicative
  to execute m1 <*> m2, first m1 is executed, resulting in a function f,
  then m2 is executed, resulting in a value x,
  and finally the value f x is returned as the result of executing m1 <*> m2.
   */

  /* TODO Haskell: Applicative from Monoid
  ((,) a) is an Applicative, as long as a is an instance of Monoid (section Monoid).
  The a values are accumulated in parallel with the computation.
   */

  /* TODO Haskell: Applicative from Const
  The Applicative module defines the Const type constructor;
  a value of type Const a b simply contains an a.
  This is an instance of Applicative for any Monoid a;
  this instance becomes especially useful in conjunction with things
  like Foldable (section Foldable).
   */

  /* TODO Haskell: Applicative from Monad or Arrow
  The WrappedMonad and WrappedArrow newtypes make any instances of Monad (section Monad)
  or Arrow (section Arrow) respectively
  into instances of Applicative;
  as we will see when we study those type classes,
  both are strictly more expressive than Applicative,
  in the sense that the Applicative methods can be implemented in terms of their methods.
   */
}


trait ApplicativeLaws[Effect[_]] {

  import Applicative.ops._

  implicit def Tested: Applicative[Effect]

  /** identity law
    *
    * Is like Functor identity but in terms of apply and pure
    *
    * Haskell:  pure id <*> v = v
    */
  def applicativeIdentity[A](fa: Effect[A]): Boolean = {
    def identityInside: Effect[A => A] =
      Tested.pure(identity)

    fa.apply(identityInside) == fa
  }

  /**
    * Homomorphism law
    *
    * applying a non-effectful function
    * to a non-effectful argument in an effectful context
    * is the same as
    * just applying the function to the argument
    * and then injecting the result into the context with pure
    *
    * apply function inside effect on value inside effect
    * is the same as
    * put inside effect result of applying f on a
    *
    * Haskell:   pure f <*> pure x = pure (f x)
    */
  def applicativeHomomorphism[A, B](a: A, f: A => B): Boolean = {
    def valueInside: Effect[A] = Tested.pure(a)
    def funInside: Effect[A => B] = Tested.pure(f)

    def fApplied: B = f(a)

    valueInside.apply(funInside) == Tested.pure(fApplied)
  }

  /**
    * Interchange law
    *
    * When evaluating the application of an effectful function
    * to a pure argument, the order in which we evaluate
    * the function and its argument doesn't matter
    *
    * Haskell:    u <*> pure y = pure ($ y) <*> u
    */
  def applicativeInterchange[A, B](a: A, ff: Effect[A => B]): Boolean = {
    def putValueUnderEffect: Effect[A] =
      Tested.pure(a)

    def applyFunctionToValueA: (A => B) => B =
      f => f(a)

    putValueUnderEffect.apply(ff) == Tested.apply(ff)(Tested.pure(applyFunctionToValueA))
  }

  /**
    *  TODO Composition Law
    *
    *  It is expressing a sort of associativity property of apply.
    *  Haskell:    u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
    */

  /**
    * Mapping a pure function g over a context
    * is the same as first injecting g into a context with pure,
    * and then applying it to context with apply
    *
    * map have to be consistent with apply and pure
    * so it specifying how Applicative should relate to Functor
    *
    * Heaskell:    fmap g x = pure g <*> x
  */
  def applicativeMap[A, B](fa: Effect[A], g: A => B): Boolean = {
    def gInside: Effect[(A) => B] =
      Tested.pure(g)

    Tested.map(fa)(g) == Tested.apply(fa)(gInside)
  }

}

object ApplicativeLaws {
  def apply[Effect[_]](implicit applicative:Applicative[Effect]): ApplicativeLaws[Effect] =
    new ApplicativeLaws[Effect] {
      val Tested = applicative
    }
}
