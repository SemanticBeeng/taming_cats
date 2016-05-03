package scalacheck_stuff

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

class StringProperties extends Properties("String") {

  property("startsWith") = forAll { (a:String, b:String) =>
    (a+b).startsWith(a) }

  property("concatenate") = forAll{ (a: String, b: String) =>
    (a+b).length >= a.length && (a+b).length >= b.length }

  property("substring") = forAll { (a: String, b: String, c:String) =>
    (a+b+c).substring(a.length, a.length + b.length) == b}

  property("endsWith") = forAll { (s1: String, s2: String) =>
    (s1 + s2).endsWith(s2) }

}
