package fun_prog_scala_runar.chapter06


trait RNG {

  def nextInt: (Int, RNG)

  def nextDouble: (Double, RNG) = {
    val (positiveValue, rng) = nextPositive
    ( (if(positiveValue != Integer.MAX_VALUE) positiveValue
       else positiveValue - 1) / Integer.MAX_VALUE.toDouble,
       rng )
  }

  def nextBoolean: (Boolean, RNG) = {
    val (intValue, rng) = nextInt
    (intValue < 0, rng)
  }

  def nextPair(): ((Int, Int), RNG) = {
    val (intValue, rng1) = nextInt
    val (intValue2, rng2) = rng1.nextInt
    ((intValue, intValue2), rng2)
  }

  def nextPositive: (Int, RNG) = {
    val (intValue, rng) = nextInt
    ( if(intValue == Integer.MAX_VALUE)
      Integer.MAX_VALUE - 1
    else intValue.abs,
      rng )
  }

  def nextIntDouble: ((Int, Double), RNG) = {
    val (intValue, rng) = nextInt
    val (doubleValue, rng2) = rng.nextDouble
    ((intValue, doubleValue), rng2)
  }

  def nextDoubleInt: ((Double, Int), RNG) = {
    val ((intValue,doubleVal), rng) = nextIntDouble
    ((doubleVal, intValue), rng)
  }

  def nextDouble3: ((Double, Double, Double), RNG) = {
    val (doubleValue, rng) = nextDouble
    val (doubleValue2, rng2) = rng.nextDouble
    val (doubleValue3, rng3) = rng2.nextDouble
    ((doubleValue, doubleValue2, doubleValue3), rng3)
  }

  def nextIntList(count:Int): (List[Int], RNG) = {
    def loop(acc:List[Int], step:Int, rng:RNG): (List[Int], RNG) =
      if(step <= 0) (acc, rng)
      else {
        val (value, rng2) = nextInt
        loop(value :: acc, step - 1, rng2)
      }
    loop(List(), count, this)
  }

}

object RNG {

  def simple(seed: Long): RNG =
    new RNG {
      def nextInt: (Int, RNG) = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ( (seed2 >>> 16).asInstanceOf[Int], simple(seed2) )
      }
    }
}
