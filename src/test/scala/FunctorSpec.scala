import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

abstract class FunctorSpec[F[_]](name:String)(
  implicit F: Functor[F],
  arbitraryInt: Arbitrary[F[Int]], // arbitrary generators
  arbitraryString: Arbitrary[F[String]],
  arbitraryDouble: Arbitrary[F[Double]] )
  extends Properties(s"Functor $name") {

  val laws = FunctorLaws[F]

  property("identity int") = forAll { (list: F[Int]) =>
    laws.identity(list)
  }

  property("identity String") = forAll { (list: F[String]) =>
    laws.identity(list)
  }

  property("identity Double") = forAll { (list: F[Double]) =>
    laws.identity(list)
  }

  property("composition int Str Double") = forAll {
    (list: F[Int],
     fa: Int => String, fb: String => Double) => laws.composition(list, fa, fb)
  }

  property("composition String") = forAll {
    (list: F[String],
     fa: String => String,
     fb: String => String) => laws.composition(list, fa, fb)
  }

  property("composition Double String Double") = forAll {
    (list: F[Double],
     fa: Double => String,
     fb: String => Double) => laws.composition(list, fa, fb)
  }

}

object ListFunctorLawSpec extends FunctorSpec[List]("List Functor")

object OptionFunctorLawSpec extends FunctorSpec[Option]("Option Functor")