
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class FunctorSpec extends Properties("Functor") {

  property("ListFunctor identity int") = forAll { (list: List[Int]) =>
    FunctorLaws(Functor.listFunctor).identity(list)
  }

  property("ListFunctor identity String") = forAll { (list: List[String]) =>
    FunctorLaws(Functor.listFunctor).identity(list)
  }

  property("ListFunctor identity Double") = forAll { (list: List[Double]) =>
    FunctorLaws(Functor.listFunctor).identity(list)
  }

  property("OptionFunctor identity int") = forAll { (list: Option[Int]) =>
    FunctorLaws(Functor.optionFunctor).identity(list)
  }

  property("OptionFunctor identity String") = forAll { (list: Option[String]) =>
    FunctorLaws(Functor.optionFunctor).identity(list)
  }

  property("OptionFunctor identity Double") = forAll { (list: Option[Double]) =>
    FunctorLaws(Functor.optionFunctor).identity(list)
  }

}
