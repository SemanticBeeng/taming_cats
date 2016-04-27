// referentially transparent
val x = "Hello world"
val r1 = x.reverse
val r2 = x.reverse

// val x = "Hello world"
val r1_1 = "Hello world".reverse
val r2_1 = "Hello world".reverse

// NOT referentially transparent
val x_2 = new StringBuilder("Hello")
val y_2 = x_2.append(" world")
val r1_2 = y_2.toString
val r2_2 = y_2.toString

val x_3 = new StringBuilder("Hello")
// val y_3 = x_3.append(" world")
val r1_3 = x_3.append(" world").toString
val r2_3 = x_3.append(" world").toString

case class Player(name: String, score: Int)

def printWinner(p: Player): Unit =
  println(p.name + " is a winner!\n")

def declareWinner(p1: Player, p2: Player): Unit =
  if (p1.score > p2.score) printWinner(p1)
  else printWinner(p2)

// separate logic of computing from side effect
// push side effect to the outer layers of the program

// fp: implement program with pure core
// and thin layer on the outside that handle side effects
val sue = Player("Sue", 7)
val bob = Player("Bob", 8)
declareWinner(sue, bob)

def winner(p1: Player, p2: Player): Player =
  if (p1.score > p2.score) p1
  else p2

def declareWinner2(p1: Player, p2: Player): Unit =
  printWinner(winner(p1, p2))

// logic could be reused
val players = List(Player("Sue", 7),
  Player("Bob", 8), Player("Joe", 4))
val p = players.reduceLeft(winner)
printWinner(p)