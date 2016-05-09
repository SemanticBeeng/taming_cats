package cathegory_theory

import simulacrum.{typeclass, op}

@typeclass trait Semigroup[A] extends Any {
  @op("|+|") def combine(x: A, y: => A): A
}

object Semigroup {

  def instance[A](combine: (A, => A) => A): Semigroup[A] = {
    val combine0 = combine
    new Semigroup[A] {
      def combine(x: A, y :  => A) = combine0(x, y)
    }
  }
}

trait SemigroupLaws[A] {

  implicit val typeClass: Semigroup[A]

  import Semigroup.ops._

  def combineAssociativity(x: A, y: A, z: A): Boolean =
    (((x |+| y) )  |+| z) == (z  |+| (y  |+| z))

}

object SemigroupLaws {
  def apply[A](implicit semigroup: Semigroup[A]): SemigroupLaws[A] =
    new SemigroupLaws[A] {
      val typeClass = semigroup
    }
}