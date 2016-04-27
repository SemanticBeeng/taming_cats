package di_using_reader_monad

/** Wrapper around unary functions with
  * map defined as andThen
  * and reasonably defined flatMap */
case class Reader[A, B](run: A => B) {

  /** Execute wrapped method with given parameter */
  def apply(x: A): B = run(x)

  /** Compose two methods */
  def map[C](f: B => C): Reader[A, C] =
    Reader(run andThen f)

  /** flatMap */
  def flatMap[C](f: B => Reader[A, C]): Reader[A,C] = ???
//    Reader(x => map(f)(x)(x))

}
