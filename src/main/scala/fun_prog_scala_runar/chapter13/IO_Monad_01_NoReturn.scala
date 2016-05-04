package fun_prog_scala_runar.chapter13

// given impure functio a => B
// can be split into functions:

// pure function a => D where D is some description
// impure functon D => B which can be thout of as interpreter of those descriptions

// can be applied repeatedly to program

// extract pure core and impreative shell around pure core

object IO_Monad_01_NoReturn {

  // //////////////////////////////////////////////////////
  /** Describe action to be executed but without executing it */
  trait IO { self =>
    def run(): Unit

    def ++(other: IO): IO = new IO { // associative operator
      override def run() = {
        self.run()
        other.run()
      }
    }
  }

  object IO {

    def empty: IO = new IO { // identity
      override def run(): Unit = ()
    }
  }

  def PrintLine(msg: String) = new  IO {
    override def run() = println(msg)
  }

  // //////////////////////////////////////////////////////
  // program:
  // //////////////////////////////////////////////////////

  case class Player(name: String, score: Int)

  // is effectfull (has/produces effect)
  def printWinner(p: Player): Unit =
    PrintLine(winnerMessage(p))

  def winnerMessage(p: Player): String =
    p.name + " is a winner!\n"

  def winner(p1: Player, p2: Player): Player =
    if (p1.score > p2.score) p1
    else p2

  def declareWinner(p1: Player, p2: Player): Unit =
    printWinner(winner(p1, p2))

  // //////////////////////////////////////////////////////
  // example
  // //////////////////////////////////////////////////////

  val sue = Player("Sue", 7)
  val bob = Player("Bob", 8)
  declareWinner(sue, bob)

  val players = List(Player("Sue", 7),
    Player("Bob", 8), Player("Joe", 4))

  val p = players.reduceLeft(winner)
  printWinner(p)

}
