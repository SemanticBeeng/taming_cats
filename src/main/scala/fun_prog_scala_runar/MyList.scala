package fun_prog_scala_runar

/* TODO check repo:
 * https://github.com/fpinscala/fpinscala/blob/master/exercises/src/main/scala/fpinscala/datastructures/List.scala
 * and answers:
 * https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/datastructures/List.scala
 */

sealed trait MyList[+A] {

  def tail: MyList[A]

  def drop(n: Int): MyList[A] = this match {
    case Nil => this
    case Cons(h, t) => if(n<=0) this else t.drop(n-1)
  }

  def dropWhile(f: A => Boolean): MyList[A] = this match {
    case Nil => this
    case Cons(h,t) => if(f(h)) t.dropWhile(f) else this
  }

//  def setHead(a: A): MyList[A] = this match { TODO contravariant arg error
//    case Nil => Cons(a, Nil)
//    case Cons(h, t) => Cons(a, t)
//  }
}

case object Nil extends MyList[Nothing] {

  override def tail: MyList[Nothing] = this
}

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // variadic function (Java.. varargs) in Scala it is Seq
  def apply[A](as: A*): MyList[A] =
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail:_*))
}
