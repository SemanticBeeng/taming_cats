package scalacheck_stuff

import org.scalacheck.{Prop, Gen, Properties}

class DataGeneratorProperty extends Properties("smallInt") {

  val smallInteger = Gen.choose(0,100)

  // dont trigger test in ~ test - maybe only for REPL?
//  val propSmallInteger = Prop.forAll(smallInteger) { n =>
//    n >= 0 && n<= 100
//  }

  property("prepend") = Prop.forAll(smallInteger) { n =>
    n >= 0 && n<= 100
  }

}
