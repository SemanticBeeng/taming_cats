package fun_prog_scala_runar.chapter10

import fun_prog_scala_runar.Monoid
import org.scalatest.{FunSuite, MustMatchers}

class WordMonoidSpec extends FunSuite with MustMatchers {

  val oper = Monoid.wordMonoid.op _

  test("returns empty string for empty string") {
    oper("", "") mustBe ""
  }

  test("jonins two string with space") {
      oper("a", "b") mustBe "a b"
  }

  test("remove space at the end") {
    oper("a", "b ") mustBe "a b"
  }

  test("remove space at the beginning") {
    oper(" a", "b") mustBe "a b"
  }

  test("dont add space if already is one") {
    oper("a ", "b") mustBe "a b"
  }

  test("example 1") {
    oper("Hic", oper("est", "chorda ")) mustBe "Hic est chorda"
    oper("Hic ", oper(" est", "chorda")) mustBe "Hic est chorda"
  }

}
