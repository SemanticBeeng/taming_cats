// first way of getting functor
val funList = Functor.listFunctor
funList.map(List(1,2,3))(_ * 1.0)
// List[Double] = List(1.0, 2.0, 3.0)

// get functor using implicitly
val funOption = implicitly[Functor[Option]]
val optMap = funOption.map( Some(1) )(_.toString)

// worksheet dies - need to run it in REPL
// console
// val funFun = implicitly[Functor[Int => ?]]
// def f = funFun.map(_ + 1)(_ * 10)
// f(2)
