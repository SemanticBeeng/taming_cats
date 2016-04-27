// pure functional (immutable)
// lineral congruential generator or random numbers

// classic technique for changing statefull API
// into pure (immutable) functional
// by instead of mutating in place state
// return new state (along with computation result)

// separate computing of next state from
// propaggating that state through program
trait RNG {

  def nextInt: (Int, RNG)

  def nextDouble: (Double, RNG) = {
    val (value, rng) = nextInt
    ( if(value != Integer.MAX_VALUE) value
      else (value - 1) / Integer.MAX_VALUE,
      rng )
  }

  def nextBoolean: (Boolean, RNG) = {
    val (value, rng) = nextInt
    (value < 0, rng)
  }

  def randomPair(): ((Int, Int), RNG) = {
    val (value1, rng1) = nextInt
    val (value2, rng2) = rng1.nextInt
    ((value1, value2), rng2)
  }

  def randomPositive: (Int, RNG) = {
    val (value, rng) = nextInt
    ( if(value == Integer.MIN_VALUE)
        Integer.MAX_VALUE
      else value.abs,
      rng )
  }
}

object RNG {

  def simple(seed: Long): RNG = new RNG(seed) {
    def nextInt: (Int, RNG) = {
      val seed2 = (seed * 0x5DEECE66DL
            + 0xBL) & ((1L << 48) - 1)
        ( (seed2 >>> 16).asInstanceOf[Int],
          simple(seed2) )
    }
  }
}

val x1 = RNG.simple(1).nextBoolean._1

val x2 = RNG.simple(1).nextBoolean._1



