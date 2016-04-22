/* // example map for Option

scala> Applicative[Option].map(Some(1))(_ + 1)
res1: Option[Int] = Some(2)

scala> Applicative[Option].map(None)(_ + 1)
<console>:12: error: value + is not a member of Nothing
  Applicative[Option].map(None)(_ + 1)
  ^

scala> Applicative[Option].map(None: Option[Int])(_ + 1)
res3: Option[Int] = None
*/

/* // example map for Option

scala> Applicative[List].map(List(1, 2, 3))(_ + 1)
res0: List[Int] = List(2, 3, 4)
*/

/* // example map2 for Option and List

scala> Applicative[Option].map2(Option(1), Option(2))(_ + _)
res0: Option[Int] = Some(3)

scala> Applicative[List].map2(List(1,2,3), List(4,5,6))(_ + _)
res1: List[Int] = List(5, 6, 7, 6, 7, 8, 7, 8, 9)
*/

/* example for map3 on Option
scala> Applicative[Option].map3(Option(1), Option(2), Option(3))(_ + _ + _)
res0: Option[Int] = Some(6)
*/

/*
scala> Applicative[Option].tuple2(Option(1), Option(2))
res0: Option[(Int, Int)] = Some((1,2))

scala> Applicative[List].tuple2(List(1,2,3), List(4,5,6))
res1: List[(Int, Int)] = List((1,4), (1,5), (1,6), (2,4), (2,5), (2,6), (3,4), (3,5), (3,6))

scala> Applicative[Option].map4(Option(1), Option(2), Option(3), Option(4))(_ + _ + _ + _)
res2: Option[Int] = Some(10)
*/

/*
scala> Applicative[List] compose Applicative[Option]
res0: Applicative[[OtherEffect]List[Option[OtherEffect]]] = Applicative$$anon$1@4bf6bbc1

scala> res0.map2(List(Some(1), None, Some(2)), List(Some(2), Some(1)))(_ + _)
res2: List[Option[Int]] = List(Some(3), Some(2), None, None, Some(4), Some(3))
*/
