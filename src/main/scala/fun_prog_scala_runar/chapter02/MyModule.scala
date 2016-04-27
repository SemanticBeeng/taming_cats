package fun_prog_scala_runar.chapter02

object MyModule {

  def abs(n: Int): Int =
    if(n < 0) -n
    else n

  private def formatResult(name: String, n: Int, f: Int => Int): String = { //  pure
    val msg = s"The %s value of %d is %d"
    msg.format(name, n, f(n))
  }

  def factorial(n: Int): Int = {

    @annotation.tailrec // compile error if cannot do TCO
    def go(n: Int, acc: Int): Int = // other common name is loop (for inner fun)
      if(n <= 0) acc
      else go(n-1, n*acc) // tail-call optimization should happen

    go(n, 1)
  }

  def fibonacci(n: Int) = { // TODO implement it (Exercise 1)
    -1
  }

  def binarySearch(ds: Array[Double], key: Double):Int = ??? // TODO 2.6 retype impl for thia and generic one

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = ??? // TODO implement using TDD

  def isSortedSpec[@specialized A](as: Array[A], gt: (A,A) => Boolean): Boolean = ??? // will produce impl for primite types too

  def main(args:Array[String]): Unit = { // side effects wraps pure function
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment", 7, x => x + 1))
    println(formatResult("increment", 7, _ + 1))

    val add1 = new Function1[Int, Int] {
      override def apply(a: Int) = a + 1
    }
    println(formatResult("increment", 7, add1))
  }

}
