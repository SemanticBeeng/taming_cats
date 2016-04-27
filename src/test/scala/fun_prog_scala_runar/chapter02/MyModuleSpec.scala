package fun_prog_scala_runar.chapter02

import org.scalatest._
import MyModule.fibonacci

class MyModuleSpec extends FunSuite with MustMatchers {

  test("fibonacci returns 0 for 1") {
    fibonacci(0) mustBe 0
  }

}
