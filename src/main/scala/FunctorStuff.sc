// first way of getting functor
val funList = Functor.listFunctor
funList.map(List(1,2,3))(_ * 1.0)
// List[Double] = List(1.0, 2.0, 3.0)

// get functor using implicitly
val funOption = implicitly[Functor[Option]]
val optMap = funOption.map( Some(1) )(_.toString)

// worksheet dies - need to run it in REPL
// go to project and run sbt
// console

/*
// examples of map without @typeclass
val funFun = implicitly[Functor[Int => ?]]
def f = funFun.map(_ + 1)(_ * 10)
f(2)
*/

/*
// examples of derived methods (with @typeclass):

scala> val funLi = Functor[List]
funLi: Functor[List] = Functor$$anon$1@b4c1568

scala> val funOp = Functor[Option]
funOp: Functor[Option] = Functor$$anon$2@92867b9

scala> import Functor.ops._
import Functor.ops._

scala> List(1, 2, 3).void
res1: List[Unit] = List((), (), ())

scala> List(1, 2, 3).as(10)
res2: List[Int] = List(10, 10, 10)

scala> List(1, 2, 3, 4, 5, 6).as(10)
res3: List[Int] = List(10, 10, 10, 10, 10, 10)
 */


/*
// example for compose
scala> val funLiOp = Functor[List] compose Functor[Option]
funLiOp: Functor[[OtherContainer]List[Option[OtherContainer]]] = Functor$$anon$1@15cedc1f

// TODO important example
scala> val xs: List[Option[Int]] = List(Some(1), None, Some(2))
xs: List[Option[Int]] = List(Some(1), None, Some(2))

scala> xs.map(_ + 1)
<console>:13: error: type mismatch;
 found   : Int(1)
 required: String
       xs.map(_ + 1)
                  ^

scala> funLiOp.map(xs)(_ + 1)
res1: List[Option[Int]] = List(Some(2), None, Some(3))
 */


