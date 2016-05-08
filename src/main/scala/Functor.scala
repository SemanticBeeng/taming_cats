import simulacrum._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

/*
 * Implementation is based on:
 * Michael Pilquist, Functional Structures in Scala:
 *  FSiS Part 1 - Type Constructors, Functors, and Kind Projector: https://www.youtube.com/watch?v=Dsd4pc99FSY
 *  FSiS Part 2 - Applicative type class: https://www.youtube.com/watch?v=tD_EyIKqqCk
 *
 * with my small changes to achive more readable code sacrificing brevity/speed/space (High hopes)
 *
 * Descriptions in comments contains:
 * - https://wiki.haskell.org/Typeclassopedia
 * - https://github.com/typelevel/cats/blob/master/docs/src/main/tut/functor.md
 */

/**
  * (Covariant) Functor - for things that can be mapped over :)
  *
  * Functor is endofunctor from Cathegory theory:
  * TODO Haskell: https://en.wikibooks.org/wiki/Haskell/Category_theory
  *
  * Abstracts over type constructor (type with type parameter like List, Option
  * not regular type like Integer or type with more than 1 parameter like Tuple)
  * In other words - for types that have one "hole" :)
  *
  * Interpretation as container:
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
  *    TODO Haskell: make instance of a generalization http://article.gmane.org/gmane.comp.lang.haskell.cafe/78052/
  *    TODO Haskell add associative constraint http://blog.omega-prime.co.uk/?p=127
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
  */
@typeclass
trait Functor[Container[_]] { self =>

  /**
    * Container interpret.: applies the function “inside” the container,
    * producing a new container.
    * Does not change the structure of container.
    *
    * Computation context interpret.: applying a function
    * to a value in a context (without altering the context)
    *
    * Haskell:
    *   class Functor f where
    *     fmap :: (a -> b) -> f a -> f b
    */
  def map[A, B](cont: Container[A])(g: A => B): Container[B]

  // ##############################################################
  // derived methods; they work the right way if map is law abiding
  // ##############################################################

  /**
    * Transforms a “normal” function g from A to B into one
    * which operates over containers/contexts Container[A] => Container[B].
    *
    * “lifts” a function from the “normal world” into the “Container/Context world”.
    *
    * for List:  lift[A, B] (f: A => B): List[A] => List[B]
    *   list function to work on lists
    *
    * for Option: lift[A, B] (f: A => B): Option[A] => Option[B]
    *   change function to work on Options (instead of raw values)
    */
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

  implicit val oneArgFunctionsFromInt: Functor[Int => ?] =
    new Functor[Int => ?] {
      def map[A, B](g: Int => A)(h: A => B): Int => B =
        g andThen h
    }

  implicit def oneArgFunctionsFromX[Input]: Functor[Input => ?] =
    new Functor[Input => ?] {
      def map[A, B](fun: Input => A)(g: A => B): Input => B =
        fun andThen g
  }

  type Tuple2SameType[T] = Tuple2[T, T]
  implicit val tupleSameTypeFunctor: Functor[Tuple2SameType] =
    new Functor[Tuple2SameType] {
      def map[A, B](tuple: Tuple2SameType[A])(g: A => B): Tuple2SameType[B] =
        (g(tuple._1), g(tuple._2))
    }

  type IntEither[T] = Either[Int, T]
  implicit val eitherFunctor: Functor[IntEither] =
    new Functor[IntEither] {
      def map[A, B](either: IntEither[A])(g: A => B): IntEither[B] =
        either match {
          case Left(ex) => Left(ex)
          case Right(value) => Right( g(value) )
        }
    }

  // TODO stream: functor

  implicit val futureFunctor: Functor[Future] =
    new Functor[Future] {
      def map[A, B](future: Future[A])(g: A => B): Future[B] =
        future.map(g)
    }

  /* TODO FPiS: reader monad functor

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

  /* TODO FPiS: IO monad functor

  IO is a Functor; a value of type IO a represents a computation producing a value of type a which may have I/O effects.
  If m computes the value x while producing some I/O effects,
  then fmap g m will compute the value g x while producing the same I/O effects.
   */

  /* TODO Haskell: how Haskell monad for container with annotation is mapped for Scala?

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
  * They ensure map
  * (container intepretation) does not change the structure of container (only elements)
  * (computation interpretation) map change the value without altering context
  *
  * Examples of things that would match the Functor signature but violate functor laws:
  * - constant function map over set would not follow rules (violate only composition)
  * - list with map that would add, remove or swat elements (violate both laws)
  *
  * Given type has at most one valid instance of Functor:
  * TODO Haskell: (2011, Russell O'Connor: http://article.gmane.org/gmane.comp.lang.haskell.libraries/15384)
  * based on free theorem:
  * TODO Haskell: (1989/1990, Philip Wadler) http://homepages.inf.ed.ac.uk/wadler/topics/parametricity.html#free
  *
  * Any Functor instance satisfying the identity law
  * will automatically satisfy composition law as well:
  * TODO Haskell: David Luposchainsky: https://github.com/quchen/articles/blob/master/second_functor_law.md
  * TODO Haskell: contrargument using undefined: http://stackoverflow.com/questions/8305949/haskell-functor-implied-law/8323243#8323243
  * https://wiki.haskell.org/Typeclassopedia#Laws
  */
trait FunctorLaws[Container[_]] {

  implicit def testedFunctor: Functor[Container]

  /** identity law:
    *
    * Mapping the identity function over every item in the container has no effect
    *
    * Haskell:   fmap id  ==  id
    */
  def identity[A](cont: Container[A]): Boolean =
    testedFunctor.map(cont)(a => a) == cont

  /** composition law
    *
    * Mapping a composition of two functions over every item in container
    * is the same as first mapping one function, and then mapping the other
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
