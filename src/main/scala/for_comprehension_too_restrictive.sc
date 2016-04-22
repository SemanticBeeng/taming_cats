/*
scala> for{ x <- Option(1); y <- Option(2); z <- Option(3) } yield x + y + z
res3: Option[Int] = Some(6)

scala> Option(1).flatMap{ x => Option(2).flatMap{ y => Option(3).map {z => z + x + y} } }
res5: Option[Int] = Some(6)

so for comprehension needs to calculate first result before proceeding with computation with Option(2)
but those are not related
this is too restrictive
*/