package fun_prog_scala_runar.chapter02

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.all
import org.scalacheck.Prop.BooleanOperators
import fun_prog_scala_runar.MathFun.fibonacci
import fun_prog_scala_runar.MathFun.abs

class MyModuleSpec extends Properties("MyModule") {

  val absAboveZero = forAll { (n: Int) =>
    abs(n).forall(_ >= 0)
  }

  val absSumOryginalIsZero = forAll { (n: Int) =>
    (n < 0) ==>
      abs(n).forall(_ + n == 0)
  }

  property("abs") = all(absAboveZero, absSumOryginalIsZero)

  property("fibonacci") = forAll { n:Int =>
    (n > 0) ==>
      (fibonacci(n) == fibonacci(n-1) + fibonacci(n-2))
  }

}
