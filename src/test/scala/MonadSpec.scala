import org.scalacheck.Prop._
import org.scalacheck.{Arbitrary, Properties}
import scala.language.higherKinds

abstract class MonadSpec[F[_]](name: String)(implicit F: Monad[F],
  arbitraryInt: Arbitrary[F[Int]],
  arbitraryString: Arbitrary[F[String]],
  arbitraryDouble: Arbitrary[F[Double]] ) extends Properties(s"Monad $name") {

  val laws = MonadLaws[F]

  property("rightIdentity int") = forAll { (cont: F[Int]) =>
    laws.rightIdentity(cont)
  }

  property("rightIdentity String") = forAll { (cont: F[String]) =>
    laws.rightIdentity(cont)
  }

  property("rightIdentity Double") = forAll { (cont: F[Double]) =>
    laws.rightIdentity(cont)
  }

  property("leftIdentity Double Int") = forAll { (value: Double, contF: Double => F[Int]) =>
    laws.leftIdentity(value, contF)
  }

  property("leftIdentity Int String") = forAll { (value: Int, contF: Int => F[String]) =>
    laws.leftIdentity(value, contF)
  }

  property("flatMapAssociavitiy Int String") = forAll { (cont: F[Int], f: Int => F[String], g: String => F[Double]) =>
    laws.flatMapAssociavitiy(cont, f, g)
  }

  property("flatMapAssociavitiy Int String") = forAll { (cont: F[String], f: String => F[Int], g: Int => F[Double]) =>
    laws.flatMapAssociavitiy(cont, f, g)
  }

  property("flatMapAssociavitiy Int String") = forAll { (cont: F[Int], f: Int => F[Double], g: Double => F[Double]) =>
    laws.flatMapAssociavitiy(cont, f, g)
  }
}

object ListMonadSpec extends MonadSpec[List]("List")

object OptionMonadSpec extends MonadSpec[Option]("Option")

//object ListOptionMonadSpec extends MonadSpec[Lambda[X => List[Option[X]]]]("List[Option]")(
//  Monad[List] compose Monad[Option],
//  implicitly, implicitly, implicitly)

//object StreamMonadSpec extends MonadSpec[Stream]("Stream Functor")
