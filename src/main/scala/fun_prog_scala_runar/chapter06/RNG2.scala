package fun_prog_scala_runar.chapter06



trait RNG2 {

  def nextInt(aRnd:RNG2): (Int, RNG2)

  def nextPositiveMax(n: Int): (Int, RNG2)

  def nextDouble(aRnd:RNG2): (Double, RNG2)

  def nextBoolean(aRnd:RNG2): (Boolean, RNG2)

  def nextPair(aRnd:RNG2): ((Int, Int), RNG2)

  def nextPositive(aRnd:RNG2): (Int, RNG2)

  def nextIntDouble(aRnd:RNG2): ((Int, Double), RNG2)

  def nextDoubleInt(aRnd:RNG2): ((Double, Int), RNG2)

  def nextDouble3(aRnd:RNG2): ((Double, Double, Double), RNG2)

  def nextIntList(aRnd:RNG2, count:Int): (List[Int], RNG2)
}

object RNG2Factory {

  def simple(seed: Long): RNG2 =
    new RNG2 {

      override def nextInt(aRnd:RNG2): (Int, RNG2) = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
      }

      type Rand[+A] = RNG2 => (A, RNG2)

      /** return constan value instead of random */
      def unit[A](a: A): Rand[A] =
        rng => (a, rng)

      def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
          val (a, rng2) = s(rng)
          (f(a), rng2)
        }

      val rInt: Rand[Int] = nextInt

      override def nextDouble(aRnd: RNG2): (Double, RNG2) =
        map[Int, Double](rInt)( a =>
          (if(a != Integer.MAX_VALUE) a
          else a - 1) / Integer.MAX_VALUE.toDouble)(aRnd)

      def nextPositiveMax(n: Int): Rand[Int] =
        map(rInt)(a => a % n)

      def nextBoolean(aRnd:RNG2): Rand[Boolean] =
        map(rInt)(_ < 0)

      def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
        rng => {
          val (valA, rngA) = f(rng)
          val (valB, rngB) = g(valA)(rngA)
          (valB, rngB)
        }

      def nextPositive(aRnd:RNG2): (Int, RNG2) = ??? // { TODO implrmrny udinh flatMap
//        val (intValue, rng) = nextInt(aRnd)
//        if(intValue == Integer.MIN_VALUE) {
//          val (rng2, intVal2) = nextInt(rng)
//          (intVal2.abs, rng2)
//        } else (intValue.abs, rng)
//      }

      def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        rng => {
          val (v1, rng2) = ra(rng)
          val (v2, rng3) = rb(rng2)
          (f(v1, v2), rng3)
        }

      val rDouble: Rand[Double] = nextDouble

      def asTuple[A, B](a: A, b: B): (A, B) = (a, b)

      def nextPair(aRnd:RNG2): Rand[(Int, Int)] =
        map2(rInt, rInt)(asTuple)

      def nextIntDouble(aRnd:RNG2): Rand[(Int, Double)] =
        map2(nextInt, nextDouble)(asTuple)

      def nextDoubleInt(aRnd:RNG2): Rand[(Double, Int)] =
        map2(rDouble, rInt)(asTuple)


      // TODO compile error
      def nextDouble3(aRnd:RNG2): ((Double, Double, Double), RNG2) = ??? //{
//        val (doubleValue, rng) = nextDouble(aRnd)
//        val (doubleValue2, rng2) = nextDouble(rng)
//        val (doubleValue3, rng3) = nextDouble(rng2)
//        ((doubleValue, doubleValue2, doubleValue3), rng3)
//      }

      def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ??? // TODO implement me

      // TODO re-implement using sequence
      def nextIntList(aRnd:RNG2, count:Int): (List[Int], RNG2) = {
        def loop(acc:List[Int], step:Int, rng: RNG2): (List[Int], RNG2) =
          if(step <= 0) (acc, rng)
          else {
            val (value, rng2) = nextInt(rng)
            loop(value :: acc, step - 1, rng2)
          }
        loop(List(), count, this)
      }
    }


}
