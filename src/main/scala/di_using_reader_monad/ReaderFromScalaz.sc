import scalaz.Reader

val f = Reader[Int, Int](_ + 2)

val g1 = f map(_ * 3)

g(3) // 15

val g2 = for(x <- f) yield x * 3

g2(3)