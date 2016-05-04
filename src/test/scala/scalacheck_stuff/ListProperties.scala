package scalacheck_stuff

import org.scalacheck.Properties
import org.scalacheck.Test.Parameters
import org.scalacheck.Prop.forAll

class ListProperties extends Properties("Examples") {

  //  property("sqrt") = forAll { (n: Int) =>
  //    scala.math.sqrt(n*n) == n
  //  }

  property("startsWith") = forAll { (a:String, b:String) =>
    (a+b).startsWith(a) }

  property("concatenate") = forAll{ (a: String, b: String) =>
    (a+b).length >= a.length && (a+b).length >= b.length }

  property("substring") = forAll { (a: String, b: String, c:String) =>
    (a+b+c).substring(a.length, a.length + b.length) == b}

  property("endsWith") = forAll { (s1: String, s2: String) =>
    (s1 + s2).endsWith(s2) }

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
