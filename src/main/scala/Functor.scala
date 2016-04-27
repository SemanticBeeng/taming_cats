import java.awt.Container

import simulacrum._
import scala.language.higherKinds

/*
 * Based on awesome video serie:
 * Functional Structures in Scala by Michael Pilquist
 * part: FSiS Part 1 - Type Constructors, Functors, and Kind Projector
 * https://www.youtube.com/watch?v=Dsd4pc99FSY
 */

/** Covariant functor */
@typeclass trait Functor[Container[_]] { // abstract over Container (type constructor)
  self => // alias for this class; usefull in compose inner trait

  /** map a function from A to B on Container with As
    * to produce Container with B's */
  def map[A, B](contA: Container[A])(funAtoB: A => B): Container[B]

  // derived methods; they work the right way if map is law abiding

  /** lift function operating of A (type of elements of container) and B
    * to function that transforms Container[A] into Container[B] */
  def lift[A, B](funAtoB: A => B): Container[A] => Container[B] =
    contA => map(contA)(funAtoB)

  /** replace content of Container[A] with with value b */
  def as[A, B](fa: Container[A], b: => B): Container[B] =
    map(fa)(_ => b)

  /** clear the content, preserving the content */
  def void[A](contA: Container[A]): Container[Unit] =
    as(contA, ()) // WTF is () empty list?

  def fproduct[A, B](contA: Container[A])(funAtoB: A => B): Container[(A,B)] = {
    def tupleArgumentWithdResult(a: A): (A, B) = (a, funAtoB(a))
    map(contA)(tupleArgumentWithdResult)
  }

  def compose[OtherFunctor[_]](implicit OtherFunctor: Functor[OtherFunctor]): // implicit ensure that OtherFun is a functor
       Functor[Lambda[OtherContainer => Container[OtherFunctor[OtherContainer]]]] = // should be
        // Functor[Container[OtherFun[?]]] but ? can't handle it
        // 33:17 more how it works
    new Functor[Lambda[OtherContainer => Container[OtherFunctor[OtherContainer]]]] {
      def map[A, B](composedCont: Container[OtherFunctor[A]])(f: A => B): Container[OtherFunctor[B]] =
        self.map(composedCont)(ga => OtherFunctor.map(ga)(a => f(a)))
      // TODO check implementation
      // self.map(composedCont)(ga => OtherFunctor.lift(f(a))
    }
}

trait FunctorLaws {

  // FSiS Part 2 https://youtu.be/tD_EyIKqqCk?list=PLFrwDVdSrYE6dy14XCmUtRAJuhCxuzJp0&t=135
  // TODO some package.scala maybe implicits in package object?
  // TODO operator =:=
  // TODO what is IsEq._
  // TODO object FunctorLaws with apply method
  // TODO why other param is not implicit?

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
}
