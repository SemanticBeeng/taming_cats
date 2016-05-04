package fun_prog_scala_runar

import fun_prog_scala_runar.Monad._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class MonadSpecification extends Properties("Monad Law") with FunctorLawSpec {

  property("listMonad Int - map identity") =
    forAll{ checkIdentityLaw[List, Int](listMonad)  }

  property("listMonad String - map identity") =
    forAll{ checkIdentityLaw[List, String](listMonad)  }

  property("optionMonad Int - map identity") =
    forAll{ checkIdentityLaw[Option, Int](optionMonad) }

  property("optionMonad String - map identity") =
    forAll{ checkIdentityLaw[Option, String](optionMonad) }

  // TODO function generator
//  property("a") = forAll { (xs: List[Int]) =>
//    listMonad.flatMap(x)(f)
//  }
  // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))


}
