import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}

abstract class ApplicativeSpec[F[_]](name: String) (
  implicit F: Applicative[F],
  arbitraryInt: Arbitrary[F[Int]],
  arbitraryString: Arbitrary[F[String]],
  arbitraryIntString: Arbitrary[F[Int => String]],
  arbitraryDoubleInt: Arbitrary[F[Double => Int]],
  arbitraryDouble: Arbitrary[F[Double]] ) extends Properties(s"Applicative $name") {

  val laws = ApplicativeLaws[F]

  property("applicativeIdentity int") = forAll { (cont: F[Int]) =>
    laws.applicativeIdentity(cont)
  }

  property("applicativeIdentity String") = forAll { (cont: F[String]) =>
    laws.applicativeIdentity(cont)
  }

  property("applicativeIdentity Double") = forAll { (cont: F[Double]) =>
    laws.applicativeIdentity(cont)
  }

  property("applicativeHomomorphism int Str") = forAll { (cont: Int, f: Int => String) =>
    laws.applicativeHomomorphism(cont, f)
  }

  property("applicativeHomomorphism Str Double") = forAll { (cont: String, f: String => Double) =>
    laws.applicativeHomomorphism(cont, f)
  }

  property("applicativeHomomorphism Double Int") = forAll { (cont: Double, f: Double => Int) =>
    laws.applicativeHomomorphism(cont, f)
  }

  property("applicativeInterchange Double Int") = forAll { (value: Double, contF: F[Double => Int]) =>
    laws.applicativeInterchange(value, contF)
  }

  property("applicativeInterchange Int String") = forAll { (value: Int, contF: F[Int => String]) =>
    laws.applicativeInterchange(value, contF)
  }

  property("applicativeMap Int String") = forAll { (cont: F[Int], f: Int => String) =>
    laws.applicativeMap(cont, f)
  }

  property("applicativeMap Int String") = forAll { (cont: F[String], f: String => Double) =>
    laws.applicativeMap(cont, f)
  }

  property("applicativeMap Int String") = forAll { (cont: F[Double], f: Double => Double) =>
    laws.applicativeMap(cont, f)
  }

}

object ListApplicativeSpec extends ApplicativeSpec[List]("List")

object OptionApplicativeSpec extends ApplicativeSpec[Option]("Option")


