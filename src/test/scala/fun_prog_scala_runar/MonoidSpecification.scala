package fun_prog_scala_runar

import fun_prog_scala_runar.Monoid._
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

trait MonoidLawSpec extends MonoidLaw {

  def checkZeroLaw[T](monoid: Monoid[T]): (T) => Boolean =
    (value: T) => zeroLaw[T](value, monoid)

  def checkOpLaw[T](monoid: Monoid[T]): (T, T, T) => Boolean =
    (value1: T, value2: T, value3: T) =>
      opLaw[T](monoid)(value1, value2, value3)
}

class MonoidSpecification extends Properties("Monoid") with MonoidLawSpec {

  // TODO macro?
  // TODO what helpers provide discipline?

  property("stringMonoid zero") = forAll { checkZeroLaw(stringMonoid) }
  property("stringMonoid op") = forAll { checkOpLaw(stringMonoid) }

  property("listMonoid zero") = forAll { checkZeroLaw[List[Int]](listMonoid) }
  property("listMonoid op") = forAll { checkOpLaw[List[Int]](listMonoid) }

  property("intAddMonoid zero") = forAll {checkZeroLaw(intAddMonoid) }
  property("intAddMonoid op") = forAll { checkOpLaw(intAddMonoid) }

  property("intMultiMonoid zero") = forAll {checkZeroLaw(intMultiMonoid) }
  property("intMultiMonoid op") = forAll { checkOpLaw(intMultiMonoid) }

  property("booleanAndMonoid zero") = forAll {checkZeroLaw(booleanAndMonoid) }
  property("booleanAndMonoid op") = forAll { checkOpLaw(booleanAndMonoid) }

  property("booleanOrMonoid zero") = forAll {checkZeroLaw(booleanOrMonoid) }
  property("booleanOrMonoid op") = forAll { checkOpLaw(booleanOrMonoid) }

  property("optionMonoid2 zero") = forAll {checkZeroLaw[Option[String]](optionMonoid2) }
  property("optionMonoid2 op") = forAll { checkOpLaw[Option[String]](optionMonoid2) }

  property("stringSpaceJoinerMonoid zero") = forAll { checkZeroLaw(wordMonoid) }
  property("stringSpaceJoinerMonoid op") = forAll { checkOpLaw(wordMonoid) }

  property("productMonoid zero") = forAll { checkZeroLaw(productMonoid(stringMonoid, intAddMonoid)) }
  property("productMonoid op") = forAll { checkOpLaw(productMonoid(stringMonoid, intAddMonoid)) }
}
