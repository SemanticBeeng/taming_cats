val daNothing: Unit = ()

// object is a namespace (but
// - can have val, def
// - can be passed around!)


val lessThan = new Function2[Int, Int, Boolean] {
  override def apply(a: Int, b: Int) = a < b
}

lessThan(1,2)

def partiall1[A,B,C](a: A, f: (A,B) => C): B => C =
  x => f(a, x)

def oneIsLessThan = partiall1(1, lessThan)
oneIsLessThan(2)

def curry[A,B,C](f: (A,B) => C): A => (B => C) =
  a => {
    b => f(a, b)
  }

def curryA = curry(lessThan)
def oneLessThanThis = curryA(1)
oneLessThanThis(2)
oneLessThanThis(0)

def uncurry[A, B, C](f: A => B => C): (A, B) => C = ???
//  a, b => f(a) TODO

def compose2[A, B, C](f: B => C, g: A => B): A => C
 = a => f(g(a))

def len(x: String) = x.length
def multi2(x: Int) = x * 2
def doublePlus1 = compose2(multi2, len)

val x = doublePlus1("ala")

val f = (x: Double) => math.Pi / 2 - x
val cos = f andThen math.sin



