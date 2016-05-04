package fun_prog_scala_runar

object MathFun {

  def abs(n: Int): Option[Int] =
    if(n >= 0) Some(n)
    else if(n > Integer.MIN_VALUE) Some(-n)
    else None

  def factorial(n: Int): Int = {

    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if(n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }


  def fibonacci(n: Int): Int = {

    @annotation.tailrec
    def loop(counter:Int, n_1:Int, n_2:Int): Int = {
      if(counter >= n) n_1 + n_2
      else loop(counter-1, n_1 + n_2, n_1)
    }

    loop(n, 0, 0)
  }

  def binarySearch(ds: Array[Double], key: Double):Int =
    ??? // TODO implement or retype from 2.6

  def isSorted[@specialized A](as: Array[A], gt: (A,A) => Boolean): Boolean =
    ??? // TODO implement
}
