package fun_prog_scala_runar.chapter02

import fun_prog_scala_runar.MathFun.factorial

object MyModule {

  private def formatResult(name: String, n: Int, f: Int => Int): String = { //  pure
  val msg = s"The %s value of %d is %d"
    msg.format(name, n, f(n))
  }

  def main(args:Array[String]): Unit = { // side effects wraps pure function
    println(formatResult("absolute value", -42, Math.abs))
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
