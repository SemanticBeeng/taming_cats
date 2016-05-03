package scalacheck_stuff

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.BooleanOperators

class ConditionalProperties extends Properties("misc stuff") {

  property("List fill") = forAll { n:Int =>
    (n >= 0 && n < 1000) ==> (List.fill(n)("").length == n)
  }

  property("will gave up") = forAll{ (n:Int) =>
    (n == 0) ==> (n == 0)
  }

}
