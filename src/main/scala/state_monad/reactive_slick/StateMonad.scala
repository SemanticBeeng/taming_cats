package state_monad.reactive_slick

// https://www.youtube.com/watch?v=WvxXz7aklik
// Reactive Slick for Database Programming
// by Stefan Zeiger

trait State[Current, Result] extends (Current => (Current, Result)) {
  self =>

  def flatMap[Other](f: Result => State[Current, Other]): State[Current, Other] =
    new State[Current, Other] {
      override def apply(s: Current): (Current, Other) = {
        val (s2, r) = self.apply(s)
        f(r)(s2)
      }
    }

  def map[Other](f: Result => Other): State[Current, Other] =
    flatMap[Other](r => State(f(r)))
}

object State {
  // lift value into State
  def apply[Current, Result](v: Result): State[Current, Result] =
    new State[Current, Result] {
      override def apply(s: Current) = (s,v)
    }

  // execute
  def run[Current, Result](s: Current, st: State[Current, Result]): Result =
    st(s)._2

  // state API

  def get[Current]: State[Current, Current] =
    new State[Current, Current] {
      override def apply(s: Current) = (s, s)
    }

  def set[Current](v: Current): State[Current, Unit] =
    new State[Current, Unit] {
      override def apply(s: Current) = (v, ())
    }
}

object ExampleUsageOfState extends App {

  val computationDescription = for {
    i <- State.get[Int]
    _ <- State.set(i + 3)
    j <- State.get
    _ <- State.set(j - 2)
    k <- State.get
  } yield k

  val evaluatedResult = State.run(41, computationDescription)
  println(evaluatedResult)
}
