package fun_prog_scala_runar

// TODO around 10.5.1 my mind exploded

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def productMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A,B)] {
      def op(first: (A, B), second: (A, B)): (A, B) =
        (a.op(first._1, second._1), b.op(first._2, second._2))

      def zero: (A, B) = (a.zero, b.zero)
    }

  def coproductMonoid[A,B](a: Monoid[A], b: Monoid[B]): Monoid[Either[A,B]] =
    ??? // TODO Either

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }

  val intAddMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }

  val intMultiMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1*a2
    def zero: Int = 1
  }

  val booleanAndMonoid = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero: Boolean = true
  }

  val booleanOrMonoid = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero: Boolean = false
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def zero: List[A] = List[A]()
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (None, a) => a
      case (b, None) => b
      case (first, _) => first // any other impl would be take the second :)
    }

    def zero: Option[A] = None
  }

  def optionMonoid2[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
      case (None, a) => a
      case (b, None) => b
      case (first, second) => second
    }

    def zero: Option[A] = None
  }

  def EndoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 andThen a2
    def zero: A => A = a => a
  }

  def wordMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String =
      (if(a1=="") a2
      else if(a2 == "") a1
      else a1.trim + " " + a2).trim

    def zero: String = ""
  }

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B) = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

}
