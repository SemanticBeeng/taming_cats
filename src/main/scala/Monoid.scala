import simulacrum.{typeclass, op}

@typeclass trait Monoid[A] extends Any with Semigroup[A] {
  def empty: A
}

object Monoid {

  def instance[A](empty: A)(combine: (A, => A) => A): Monoid[A] = {
    val combine0 = combine
    val empty0 = empty
    new Monoid[A] {
      def combine(x: A, y :  => A) = combine0(x, y)
      def empty = empty0
    }
  }

}
