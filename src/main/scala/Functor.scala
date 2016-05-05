import simulacrum._
import scala.language.higherKinds

/*
 * Based on awesome video serie:
 * Functional Structures in Scala by Michael Pilquist
 * FSiS Part 1 - Type Constructors, Functors, and Kind Projector
 *   https://www.youtube.com/watch?v=Dsd4pc99FSY
 * FSiS Part 2 - Applicative type class
 *   https://youtu.be/tD_EyIKqqCk
 */

/** Covariant functor */
@typeclass
trait Functor[Container[_]] { self =>

  /** map a function from A to B on Container with As
    * to produce Container with B's */
  def map[A, B](contA: Container[A])(funAtoB: A => B): Container[B]

  // derived methods; they work the right way if map is law abiding

  /** lift function operating of A (type of elements of container) and B
    * to function that transforms Container[A] into Container[B]
    *
    * for list:  lift[A, B] (f: A => B): List[A] => List[B]
    *   promote function to work on lists
    *
    * for option: lift[A, B] (f: A => B): Option[A] => Option[B]
    *   change function to work on Options (instead of arguments)
    * */
  def lift[A, B](f: A => B): Container[A] => Container[B] =
    (contA: Container[A])
      => map(contA)(f)

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
  def fproduct[A, B](contA: Container[A])(f: A => B): Container[(A,B)] = {
    def helper(a: A): (A, B) =
      (a, f(a))

    map(contA)(helper)
  }

  def zipWith[A, B](contA: Container[A])(f: A => B): Container[(A,B)] = fproduct[A, B](contA)(f)

  /** Allow for composition of Functors (List of Option of ....) */
  def compose[OtherFunctor[_]](implicit OtherFunctor: Functor[OtherFunctor]): // implicit - ensure OtherFun is functor
       Functor[Lambda[OtherContainer => Container[OtherFunctor[OtherContainer]]]] =
          // should be Functor[Container[OtherFun[?]]] but scala can't handle

    new Functor[Lambda[OtherContainer => Container[OtherFunctor[OtherContainer]]]] {
      def map[A, B](composedCont: Container[OtherFunctor[A]])(f: A => B): Container[OtherFunctor[B]] =
        self.map(composedCont)(ga => OtherFunctor.map(ga)(a => f(a)))
    }
}

object Functor {

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](list: List[A])(fun: A => B): List[B] =
      list.map(fun)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](option: Option[A])(fun: (A) => B): Option[B] =
      option.map(fun)
  }

  //  type OneArgFun[X] = Function1[X, _]
  implicit def oneArgFunctionResultFunctor[X]: Functor[X => ?] = new Functor[X => ?] {
    override def map[A, B](oneArgFun: X => A)(funAtoB: A => B): X => B =
      oneArgFun andThen funAtoB
  }

  // TODO stream functor

  // TODO future functor
}

trait FunctorLaws[Container[_]] {

  implicit def testedFunctor: Functor[Container]

  // identity law
  def identity[A](contA: Container[A]): Boolean =
    testedFunctor.map(contA)(a => a) == contA

  // composition law
  def composition[A, B, C](
          contA: Container[A],
          funFirstAtoB: A => B, funSecondBtoC: B => C): Boolean = {
    val contMapped = testedFunctor.map(contA)(funFirstAtoB)
    val composedFirstWithSecond = funFirstAtoB andThen funSecondBtoC
    testedFunctor.map(contMapped)(funSecondBtoC) == testedFunctor.map(contA)(composedFirstWithSecond)
  }
}

object FunctorLaws {
  def apply[Container[_]](implicit functor:Functor[Container]): FunctorLaws[Container] =
    new FunctorLaws[Container] {
      def testedFunctor = functor
    }
}
