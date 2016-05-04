package fun_prog_scala_runar.chapter13

// TODO implement monad
//  https://github.com/fpinscala/fpinscala/blob/master/exercises/src/main/scala/fpinscala/iomonad/Monad.scala
// TODO https://github.com/fpinscala/fpinscala/blob/master/exercises/src/main/scala/fpinscala/iomonad/IO.scala


//import cats.Monad

object IO_Monad_02_Return extends App {

  def fahrenheitToCelsius(temp: Double): Double =
    (temp - 32) * 5.0/9.0

  /////////////////////////////////////
  // approach with mix of side effects and logic:
  /*
  def converter(): Unit = {
    println("Enter temp in fahrenheit:")
    val d = readLine.toDouble
    println(fahrenheitToCelsius(d))
  }
  */

  /////////////////////////////////////
  // approach with separation of side effects and logic using IO monad:

  trait IO[+InnerFunResult] { self =>
    def run: InnerFunResult

    def map[NewInnerFunResult](f: InnerFunResult => NewInnerFunResult): IO[NewInnerFunResult] =
      new IO[NewInnerFunResult] {
        def run = f(self.run)
      }

    def flatMap[NewInnerFunResult](f: InnerFunResult => IO[NewInnerFunResult]): IO[NewInnerFunResult] =
      new IO[NewInnerFunResult] {
        def run = f(self.run).run
      }
  }

  object IO /* extends Monad[IO] */ {

    def flatMap[InnerFunResult, NewInnerFunResult]
      (fa: IO[InnerFunResult])
      (f: InnerFunResult => IO[NewInnerFunResult]):
        IO[NewInnerFunResult] =
      fa flatMap f

    def pure[InnerFunResult](a: => InnerFunResult): IO[InnerFunResult] =
      new IO[InnerFunResult] {
        override def run: InnerFunResult = a
      }

    def apply[InnerFunResult](a: => InnerFunResult): IO[InnerFunResult] =
      pure(a)
  }

  def ReadLine: IO[String] = IO { readLine } // TODO shouldnt it be run = .... ???
  def PrintLine(msg: String): IO[Unit] = IO { print(msg) } // TODO as above

  def convert: IO[Unit] = for {
    _ <- PrintLine("Enter temprature in fehrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  //////////////////////////////////////

  val echo = ReadLine.flatMap(PrintLine)
  val readInt: IO[Int] = ReadLine.map(_.toInt)
//  val readInts: IO[(Int, Int)] = readInt ** readInt


}
