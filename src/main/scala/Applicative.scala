/*
 * Based on awesome video serie: Functional Structures in Scala
 * by: Michael Pilquist
 * part: FSiS Part 2 - Applicative type class
 * https://www.youtube.com/watch?v=tD_EyIKqqCk
 */

import simulacrum._

/**
  * Applicative functor.
  *
  * map apply function in context (effect)
  * pure put in context (effect)
  *
  * More stuff:
  *  The Essence of the Iterator Pattern: https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf
  *  Applicative programming with effects: http://staff.city.ac.uk/~ross/papers/Applicative.pdf
  *  Move Over Free Monads: Make Way for Free Applicatives! — John de Goes: https://www.youtube.com/watch?v=H28QqxO7Ihc
  *
  * Effect might not be side effect
  * Examples of effects:
  *   Option - effect of having a value or not having a value
  *   LIst - effect of having multiple values
  */
@typeclass trait Applicative[Effect[_]] extends Functor[Effect] {

  /**
    * Take value and put (lift) into effect
    */
  def pure[A](value: A): Effect[A]
}
