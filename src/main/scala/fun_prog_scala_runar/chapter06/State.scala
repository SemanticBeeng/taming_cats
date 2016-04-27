package fun_prog_scala_runar.chapter06

// fist definition
object StateAction {
  type StateAction[S, +A] = S => (A, S)
}

// second defintion using case class

case class State[S, +A](run: S => (A,S)) {

  def unit[A](a: A): State[S, A] =
    State(s => (a, s))

  def map[A, B](s: State[S, A])(f: A => B): State[S, B] =
    State(rng => {
      val (a, rng2) = s.run(rng)
      (f(a), rng2)
    })

  def flatMap[A, B](f: State[S, A])(g: A => State[S, B]): State[S, B] =
    State(rng => {
      val (valA, rngA) = f.run(rng)
      val (valB, rngB) = g(valA).run(rngA)
      (valB, rngB)
    })


  def map2[A, B, C](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(rng => {
      val (v1, rng2) = ra.run(rng)
      val (v2, rng3) = rb.run(rng2)
      (f(v1, v2), rng3)
    })

  def sequence[A](fs: List[State[S, A]]): State[S, List[A]] = ??? // TODO implement me

}
