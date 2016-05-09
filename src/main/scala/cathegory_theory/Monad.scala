package cathegory_theory

import simulacrum._
import scala.language.higherKinds

/*
 * Based on awesome video serie:
 * Functional Structures in Scala by Michael Pilquist
 * FSiS Part 3 - Monad type class
 *   https://www.youtube.com/watch?v=VWCtLhH815M
 */

/**
  * Adds to the operations of Functor - map and Applicative - apply, pure method flatMap
  *
  * It combines two computations into one larger computation.
  * apply joins two computations under effect.
  * flatMap given a result of the first computation,
  * can produce a second computation to be run.
  * In other words computation which runs x, and then uses the result(s) of x
  * to decide what computation to run second,
  * using the output of the second computation as the result of the entire computation.
  *
  * It is this ability to use the output from previous computations to decide
  * what computations to run next that makes Monad more powerful than Applicative.
  *
  * The structure of an Applicative computation is fixed,
  * whereas the structure of a Monad computation can change based on intermediate results.
  *
  * This also means that parsers built using an Applicative interface can only
  * parse context-free languages; in order to parse context-sensitive languages
  * a Monad interface is needed
  *
  * Minimal set of methods to implement others (minimal set of combinators):
  *  - pure, flatMap
  *  - pure, compose
  *  - pure, flatten, map
  *  - pure, flatten, apply
  *  - pure, flatten, map2
  *
  *  for example Option Monad represents computation that may fail:
  *    if we build up a computation by chaining together a bunch of functions with flatMap,
  *    as soon as any one of them fails, the entire computation will fail.
  *    The entire computation succeeds only if
  *    all the constituent functions individually succeed.
  */

@typeclass
trait Monad[Effect[_]] extends Applicative[Effect] { self =>

  /**
    * other names: return, unit
    */
  def pure[A](value: A): Effect[A]

  /**
    * other names: bind, >>=
    */
  def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B]

  override def apply[A, B](fa: Effect[A])(ff: Effect[A => B]): Effect[B] = {
    val helper: (A => B) => Effect[B] =
      (f: A => B) => map(fa)(f)

    flatMap(ff)(helper)
  }

  override def map[A, Z](fa: Effect[A])(f: A => Z): Effect[Z] =
    flatMap(fa)(a => pure(f(a)))

//  def compose[Other[_]](implicit OtherApplicative: Monad[Other]):
//    Monad[Lambda[OtherEffect => Effect[Other[OtherEffect]]]] =
//    ??? // cant be done for any monad, yet can be done for Option, List,

//  def filter[A, B](fa: Effect[A])(f: A => Boolean): Effect[A] =
//    ??? // impossible with given implementation

  // derived methods

  /**
    * Provides ability to collapse multiple effects into one
    *
    * other names: join
    * Haskell:   join :: m (m a) -> m a
    */
  def flatten[A](ffa: Effect[Effect[A]]): Effect[A] =
    flatMap(ffa)(identity)

  def sequence[A](lma: List[Effect[A]]): Effect[List[A]] =
    traverse(lma)(ma => ma)

  def traverse[A, B](la: List[A])(f: A => Effect[B]): Effect[List[B]] =
    la.foldRight( pure(List[B]()) )( (a, mbs) => map2(f(a), mbs)(_ :: _))

  def replicateEffect[A](n: Int, ma: Effect[A]): Effect[List[A]] =
    map(ma)( (a:A) => List.fill(n)(a) )

  def factor[A, B](ma: Effect[A], mb: Effect[B]): Effect[(A, B)] =
    map2(ma, mb)((_, _))
}

object Monad {

  implicit val listMonad = new Monad[List] {

    def pure[A](value: A): List[A] = List(value)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa flatMap f
  }

  implicit val optionMonad = new Monad[Option] {
    def pure[A](value: A): Option[A] = Option(value)

    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa flatMap f
  }

  // TODO identity monad
  // TODO 2007, Dan Piponi: http://blog.sigfpe.com/2007/04/trivial-monad.html

  /* TODO FPiS: IO Monad
   *
   * It allows to build up, in an entirely pure way, values
   * representing possibly effectful computations.
   *
   * We often speak of monadic values as “effectful computations”,
   * but this is because some monads allow us to write code
   * as if it has side effects, when in fact the monad
   * is hiding the plumbing which allows
   * these apparent side effects to be implemented
   * in a functionally pure way.
   */

  /* TODO Reader Monad
   * As mentioned earlier, ((->) e) is known as the reader monad,
   * since it describes computations in which a value of type e
   * is available as a read-only environment.
   * Reader-specific utility functions such as ask (retrieve the environment),
   * asks (retrieve a function of the environment),
   * and local (run a subcomputation under a different environment).
   */

 /* TODO Writer Monad
  *  Writer monad, allows information to be collected as a computation progresses.
  *  Writer w a is isomorphic to (a,w), where the output value a
  *  is carried along with an annotation or “log” of type w,
  *  which must be an instance of Monoid (see section Monoid);
  *  the special function tell performs logging.
  */

  /* TODO State Monad
   * Something of type State s a represents a stateful computation
   * which produces an a but can access and modify the state of type s
   * along the way. The module also provides State-specific utility functions
   * such as get (read the current state),
   * gets (read a function of the current state),
   * put (overwrite the state),
   * and modify (apply a function to the state).
   */

  /* TODO Cont(inuation passing) Monad
   *
   * Cont monad, which represents computations in continuation-passing style.
   * It can be used to suspend and resume computations,
   * and to implement non-local transfers of control, co-routines,
   * other complex control structures—all in a functionally pure way.
   * Cont has been called the “mother of all monads” because of its universal properties.
   */

  /* TODO free monad
   * Implement Functor and Monad instances for Free f, defined as

    data Free f a = Var a
                | Node (f (Free f a))

   * You may assume that f has a Functor instance.
   * This is known as the free monad built from the functor f.
   */

}

trait MonadLaws[Effect[_]] {

  import Monad.ops._

  implicit val Tested: Monad[Effect]

  def flatMapAssociavitiy[A, B, C](fa: Effect[A], f: A => Effect[B], g: B => Effect[C]): Boolean =
    fa.flatMap(f).flatMap(g) == fa.flatMap { a => f(a).flatMap( b => g(b) ) }

  def leftIdentity[A, B](a: A, f: A => Effect[B]): Boolean =
    Tested.pure(a).flatMap(f) == f(a)

  def rightIdentity[A](fa: Effect[A]): Boolean =
    fa.flatMap{ a => Tested.pure(a) } == fa

}

object MonadLaws {
  def apply[Effect[_]](implicit monad:Monad[Effect]): MonadLaws[Effect] =
    new MonadLaws[Effect] {
      val Tested = monad
    }
}
