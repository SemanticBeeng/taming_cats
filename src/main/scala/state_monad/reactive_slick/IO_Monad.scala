package state_monad.reactive_slick

// http://slick.typesafe.com/talks/scaladays2015sf/Reactive_Slick_for_Database_Programming.pdf
// https://www.youtube.com/watch?v=WvxXz7aklik
// Reactive Slick for Database Programming
// by Stefan Zeiger



trait IO[Result] extends (DB => Result) { self =>

  def flatMap[Other](f: Result => IO[Other]): IO[Other] =
    new IO[Other] {
      def apply(db: DB) = f(self.apply(db))(db)
    }
  //    new FlatMapIO[Other](f)

  def map[Other](f: Result => Other): IO[Other] = ???
//    new IO[Other] {
//      def apply(db: DB) = f(self.apply(db))(db)
//    }

}

//class FlatMapIO[Result, Other](f: Result => IO[Other]) extends IO[Other]

object IO {
  /*
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
   */


  def get(): IO[Int] =
    new IO[Int] {
      def apply(db: DB) = db.mutableState
    }

  def set(v: Int): IO[Unit] =
    new IO[Unit] {
      def apply(db: DB) = db.mutableState = v
    }
}

class DB(var mutableState: Int) {
  def run[Result](io: IO[Result]): Result = io(this)
}

object ExampleUsageOfIOMonad extends App {
  val io = for {
    i <- IO.get
    _ <- IO.set(i + 3)
    j <- IO.get
    _ <- IO.set(j - 2)
    k <- IO.get
  } yield k

  val result = new DB(41).run(io)
  println(result)
}
