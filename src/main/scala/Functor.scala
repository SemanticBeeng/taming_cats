import simulacrum._
import scala.language.higherKinds

/*
 * Based on awesome video serie:
 * Functional Structures in Scala by Michael Pilquist
 * part: FSiS Part 1 - Type Constructors, Functors, and Kind Projector
 * https://www.youtube.com/watch?v=Dsd4pc99FSY
 */

/** Covariant functor */
trait Functor[Container[_]] { // abstract over Container (type constructor)

  /** map a function from A to B on Container with As
    * to produce Container with B's */
  def map[A, B](contA: Container[A])(funAtoB: A => B): Container[B]

  // derived methods

  /** lift function operating of A (type of elements of container) and B
    * to function that transforms Container[A] into Container[B] */
  def lift[A, B](funAtoB: A => B): Container[A] => Container[B] =
    contA => map(contA)(funAtoB)

  /** replace content of Container[A] with with value b */
  def as[A, B](fa: Container[A], b: => B): Container[B] =
    map(fa)(_ => b)

  /** clear the content, preserving the content */
  def void[A](contA: Container[A]): Container[Unit] =
    as(contA, ()) // WTF is ()

  // TODO fproduct

  // TODO compose
}

trait FunctorLaws {

  // identity law
  def identity[Container[_], A](contA: Container[A])(implicit testedFunctor: Functor[Container]) =
    testedFunctor.map(contA)(a => a) == contA

  // composition law
  def composition[Container[_], A, B, C](
          contA: Container[A],
          funFirstAtoB: A => B,
          funSecondBtoC: B => C)(implicit testedFunctor: Functor[Container]) = {
    val contMapped = testedFunctor.map(contA)(funFirstAtoB)
    val composedFirstWithSecond = funFirstAtoB andThen funSecondBtoC
    testedFunctor.map(contMapped)(funSecondBtoC) == testedFunctor.map(contA)(composedFirstWithSecond)
  }
}

// some instances of functor
object Functor {

  implicit val listFunctor: Functor[List] = new Functor[List] {
    def map[A, B](list: List[A])(fun: A => B): List[B] = list.map(fun)
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](option: Option[A])(fun: (A) => B): Option[B] = option.map(fun)
  }

//  type OneArgFun[X] = Function1[X, _]
  implicit def oneArgFunctionResultFunctor[X]: Functor[X => ?] = new Functor[X => ?] {
    override def map[A, B](oneArgFun: X => A)(funAtoB: A => B): X => B =
      oneArgFun andThen funAtoB
  }
}
