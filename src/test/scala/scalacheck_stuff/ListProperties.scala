package scalacheck_stuff

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class ListProperties extends Properties("List") {

  property("prepend") = forAll { (firstList: List[Int], secondList: List[Int]) =>
    firstList.size + secondList.size == (firstList ::: secondList).size }

  // example of function and inverse function
  property("reverse") = forAll { (list: List[String]) =>
    list.reverse.reverse == list }

  // example of invariant
  property("map") = forAll { (list: List[Int]) =>
    list.map(a => a) == list
  }

  property("filter all") = forAll { (list:List[Int]) =>
    list.filter(_ => false) == Nil
  }

  property("filter none") = forAll { (list:List[Int]) =>
    list.filter(_ => true) == list
  }

  // idempotence invariant
  property("dictinct idempotence") = forAll{ (list: List[Int]) =>
    list.distinct.distinct == list.distinct }

  property("sorted idempotence") = forAll{ (list: List[Int]) =>
    list.sorted.sorted == list.sorted }
}
