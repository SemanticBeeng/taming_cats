import simulacrum._
import scala.language.higherKinds

/** Covariant functor */
trait Functor[Container[_]] { // abstract over Container (type constructor)

  /** map a function from A to B on Container with As
    * to produce Container with B's */
  def map[A, B](contA: Container[A])(funAtoB: A => B): Container[B]

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