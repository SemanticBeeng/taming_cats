
case class State[S, +A](runS: S => (A,S)) {
  def map[B](f: A => B) =
    State[S, B](s => {
      val (a, s1) = runS(s)
      (f(a),s1)
    })
  def flatMap[B](f: A => State[S,B]) =
    State[S,B](s => {
      val (a, s1) = runS(s)
      f(a) runS s1
    })
}