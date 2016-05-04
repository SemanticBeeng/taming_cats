package fun_prog_scala_runar

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import fun_prog_scala_runar.Functor._

import scala.language.higherKinds

trait FunctorLaw {
  def id[A](a: A): A = a

  def identityLaw[T[_], A](ta: T[A], functor: Functor[T]): Boolean =
    functor.map(ta)(id) == ta

  // TODO map associativity law
}

trait FunctorLawSpec extends FunctorLaw {

  def checkIdentityLaw[T[_], A](functor: Functor[T]): (T[A]) => Boolean =
    (ta: T[A]) =>  identityLaw[T, A](ta, functor)

}

class FunctorSpecificiation extends Properties("Functor") with FunctorLawSpec {
  property("list functor Int law") = forAll{ checkIdentityLaw[List, Int](listFunctor)  }
  property("list functor String law") = forAll{ checkIdentityLaw[List, String](listFunctor)  }

  property("option functor Int law") = forAll{ checkIdentityLaw[Option, Int](optionFunctor) }
  property("option functor String law") = forAll{ checkIdentityLaw[Option, String](optionFunctor) }
}

