import simulacrum._
import scala.concurrent.Future
import scala.language.higherKinds

/*
 * Implementation is based on:
 * Michael Pilquist, Functional Structures in Scala, Part (1) and (2)
 */

/**
  * Covariant functor
  *
  * Abstracts over type constructor (generic type like List, Option
  * not regular type like Integer).
  *
  * Interpretation as container (3):
  *
  * Functor as container
  * ====================
  * Functor abstract over (represents):
  *  - container (Container[_]) and
  *  - ability (map) to apply function (f) to every element of that container
  *
  * For example:
  *  - list, tree, map - collection of elements and apply function to every element (List.map)
  *  - set is not a Functor (map with function that returns const value)
  *    TODO make instance of a generalization http://article.gmane.org/gmane.comp.lang.haskell.cafe/78052/
  *    TODO add associative constraint http://blog.omega-prime.co.uk/?p=127
  *  - option - container that can contain single value or may be empty and ability
  *     to apply function to its element or ignore function if it does not contain any function
  *
  * Functor as computational context
  * ================================
  * Functor represents (abstracts over) computational context (Container) and ability to
  *  apply function to a value (of type A) without changing the context
  *
  * For example:
  *   list - nondeterministic choice: single element can have many possible values (elements of the list)
  *   option - possible failure
  *
  * =====
  * (1) FSiS Part 1 - Type Constructors, Functors, and Kind Projector https://www.youtube.com/watch?v=Dsd4pc99FSY
  * (2) FSiS Part 2 - Applicative type class https://www.youtube.com/watch?v=tD_EyIKqqCk
  * (3) https://wiki.haskell.org/Typeclassopedia#Functor
  * */
@typeclass
trait Functor[Container[_]] { self =>

  /**
    * Apply function f to every element of container cont
    * Output is container of results.
    *
    * Does not change the structure of container.
    *
    * Haskell:
    *   class Functor f where
    *     fmap :: (a -> b) -> f a -> f b
    */
  def map[A, B](cont: Container[A])(g: A => B): Container[B]

  // ~~@@@^^^^#####################################################
  // derived methods; they work the right way if map is law abiding
  // ~~@@@^^^^#####################################################

  /** lift function operating of A (type of elements of container) and B
    * to function that transforms Container[A] into Container[B]
    *
    * for list:  lift[A, B] (f: A => B): List[A] => List[B]
    *   promote function to work on lists
    *
    * for option: lift[A, B] (f: A => B): Option[A] => Option[B]
    *   change function to work on Options (instead of arguments)
    * */
  def lift[A, B](g: A => B): Container[A] => Container[B] =
    (contA: Container[A])
      => map(contA)(g)

  /** replace content of Container[A] with with value b
    *
    * for List:  as[A, B] (fa: List[A], b: => B): List[B]
    *    ignore existing elements in List and just put constant value inside List
    *
    * for Option:  as[A, B](fa: Option[A], b: => B): Option[B]
    *    ignore existing value and just put constant value inside Option
    * */
  def as[A, B](fa: Container[A], b: => B): Container[B] =
    map(fa)(_ => b)

  /** clear the content, preserving the structure
    *
    * for List: void[A] (contA: List[A]): List[Unit]
    *    create list with void values
    *
    * for Option: void[A](contA: Option[A]): Option[Unit]
    *    create Option with void value
    * */
  def void[A](contA: Container[A]): Container[Unit] =
    as(contA, ())

  /**
    * take function and create Container with pair with element and result of invoking this function
    *
    * for Option: fproduct[A, B](option: Option[A])(f: A => B): Option[(A,B)]
    *   return list of pairs (value, f(value))
    *
    * for List: fproduct[A, B](list: List[A])(f: A => B): List[(A,B)]
    *   return Option with pair (value, f(value))
    */
  def fproduct[A, B](contA: Container[A])(g: A => B): Container[(A,B)] = {
    def helper(a: A): (A, B) =
      (a, g(a))

    map(contA)(helper)
  }

  def zipWith[A, B](contA: Container[A])(g: A => B): Container[(A,B)] = fproduct[A, B](contA)(g)

  /** Allow for composition of Functors (List of Option of ....) */
  def compose[OtherFunctor[_]](implicit OtherFunctor: Functor[OtherFunctor]): // implicit - ensure OtherFun is functor
       Functor[Lambda[OtherContainer => Container[OtherFunctor[OtherContainer]]]] =
          // should be Functor[Container[OtherFun[?]]] but scala can't handle

    new Functor[Lambda[OtherContainer => Container[OtherFunctor[OtherContainer]]]] {
      def map[A, B](composedCont: Container[OtherFunctor[A]])(g: A => B): Container[OtherFunctor[B]] =
        self.map(composedCont)(ga => OtherFunctor.map(ga)(a => g(a)))
    }
}

object Functor {

  implicit val listFunctor: Functor[List] =
    new Functor[List] {
      def map[A, B](list: List[A])(g: A => B): List[B] =
        list.map(g)
    }

  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def map[A, B](option: Option[A])(g: A => B): Option[B] =
        option.map(g)
    }

  // functions that takes one argument of type Int: Function1[Int, _]
  // use syntax from kind-projector
  implicit val oneArgFunctionsFromInt: Functor[Int => ?] =
    new Functor[Int => ?] {
      def map[A, B](g: Int => A)(h: A => B): Int => B =
        g andThen h
    }

  // functions that take one argument of some type X: Function1[X, _]
  // use syntax from kind-projector
  implicit def oneArgFunctionsFromX[X]: Functor[X => ?] =
    new Functor[X => ?] {
      def map[A, B](g: X => A)(h: A => B): X => B =
        g andThen h
  }

  type MyTuple1[T] = Tuple2[T, T] // TODO use kind-projector syntactic sugar
  implicit val tupleSameTypeFunctor: Functor[MyTuple1] =
    new Functor[MyTuple1] {
      def map[A, B](tuple: MyTuple1[A])(g: A => B): MyTuple1[B] =
        (g(tuple._1), g(tuple._2))
    }

  // TODO Functor for Tuple and generic parameter

  type IntEither[T] = Either[Int, T] // TODO use kind-projector syntactic sugar
  implicit val eitherFunctor: Functor[IntEither] =
    new Functor[IntEither] {
      def map[A, B](either: IntEither[A])(g: A => B): IntEither[B] =
        either match {
          case Left(ex) => Left(ex)
          case Right(value) => Right( g(value) )
        }
    }

  // TODO Functor for Either with generic parameter

  // TODO stream functor

  implicit val futureFunctor: Functor[Future] =
    new Functor[Future] {
      def map[A, B](future: Future[A])(g: A => B): Future[B] =
        future.map(g)
    }

  /* TODO reader monad functor

  ((->) e) (which can be thought of as (e ->); see above),
  the type of functions which take a value of type e as a parameter, is a Functor.

  As a container, (e -> a) represents a (possibly infinite) set of values of a,
  indexed by values of e.

  Alternatively, and more usefully,
  ((->) e) can be thought of as a context in which a value of type e
  is available to be consulted in a read-only fashion.

  This is also why ((->) e) is sometimes referred to as the reader monad;
  more on this later.
  */

  /* TODO IO monad functor

  IO is a Functor; a value of type IO a represents a computation producing a value of type a which may have I/O effects.
  If m computes the value x while producing some I/O effects,
  then fmap g m will compute the value g x while producing the same I/O effects.
   */

  /* TODO how Haskell monad for container with annotation is mapped for Scala?

  ((,) e) represents a container which holds an “annotation” of type e along with the actual value it holds.
  It might be clearer to write it as (e,), by analogy with an operator section like (1+),
  but that syntax is not allowed in types
  (although it is allowed in expressions with the TupleSections extension enabled).
  However, you can certainly think of it as (e,).
   */
}

/**
  * Rules for map function of Functor
  *
  * https://hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#t:Functor
  */
trait FunctorLaws[Container[_]] {

  implicit def testedFunctor: Functor[Container]

  /** identity law:
    *
    * if you use identity function to map over elemnts in Container
    * then you get the same container
    *
    * Haskell:   fmap id  ==  id
    */
  def identity[A](cont: Container[A]): Boolean =
    testedFunctor.map(cont)(a => a) == cont

  /** composition law
    *
    * For any two functions g and h it does not matter
    * if you combine them and then map over container or
    * map over container with g and then map over result container with h
    *
    * Haskell:    fmap (f . g)  ==  fmap f . fmap g
    */
  def composition[A, B, C](
        cont: Container[A],
        g: A => B, h: B => C): Boolean = {
    val contMapped: Container[B] = testedFunctor.map(cont)(g)
    def composedGH: A => C = g andThen h
    testedFunctor.map(contMapped)(h) == testedFunctor.map(cont)(composedGH)
  }
}

object FunctorLaws {
  def apply[Container[_]](implicit functor:Functor[Container]): FunctorLaws[Container] =
    new FunctorLaws[Container] {
      def testedFunctor = functor
    }
}
