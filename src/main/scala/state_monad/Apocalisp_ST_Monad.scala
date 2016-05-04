package state_monad

// based on: https://apocalisp.wordpress.com/2011/03/20/towards-an-effect-system-in-scala-part-1/

case class World[A]()

case class ST[S, A](f: World[S] => (World[S], A)) {
  def apply(s: World[S]) = f(s)
  def flatMap[B](g: A => ST[S, B]): ST[S, B] = ???
}
