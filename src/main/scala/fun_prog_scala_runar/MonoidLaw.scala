package fun_prog_scala_runar

trait MonoidLaw {

  def zeroLaw[T](value: T, monoid: Monoid[T]): Boolean =
    (monoid.op(monoid.zero, value) == value) &&
      (monoid.op(value, monoid.zero) == value)

  def opLaw[T](monoid: Monoid[T])(value1: T, value2: T, value3: T): Boolean =
    monoid.op(value1, monoid.op(value2, value3)) ==
      monoid.op(monoid.op(value1, value2), value3)

}
