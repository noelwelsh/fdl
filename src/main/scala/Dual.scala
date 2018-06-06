package fdl

/**
  * One-dimensional dual number, implementing forward-mode automatic
  * differentiation.
  *
  * In this implementation a Dual number can contain any value, not just a
  * number, along with a derivative (which is restricted to being a `Double`).
  * This unusual choice means that `flatMap` can be implemented, which provides
  * the chain rule. This has benefits and drawbacks: implementing composition is
  * simpler but it is possible to implement some computations incorrectly in
  * terms of `flatMap`. For example, multiplication can be incorrectly
  * implemented in terms of `flatMap` as shown below.
  *
  * {{{
  * // Correct
  * Dual(2.0) * Dual(2.0)
  * // Dual(4.0, 4.0)
  *
  * // Incorrect
  * for {
  *  a <- Dual(2)
  *  b <- Dual(2)
  * } yield a * b
  * // Dual(4.0, 1.0)
  * }}}
  */
final case class Dual[A](value: A, derivative: Double) {
  /** flatMap is the chain rule for dual numbers. */
  def flatMap[B](f: A => Dual[B]): Dual[B] = {
    val next = f(value)
    val nextD = this.derivative * next.derivative
    Dual(next.value, nextD)
  }

  def map[B](f: A => B): Dual[B] =
    Dual(f(value), derivative)

  // Binary operations
  //
  // Most of these should be considered primitives that can't be defined in
  // terms of other operations.

  def *(that: Dual[A])(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = this.value * that.value
    val nextD = (this.value * that.derivative) + (this.derivative * that.value)
    Dual(nextV, nextD)
  }

  def /(that: Dual[A])(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = this.value / that.value
    val nextD = ((this.derivative * that.value) - (this.derivative * that.value)) / (that.value * that.value)
    Dual(nextV, nextD)
  }

  def +(that: Dual[A])(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = this.value + that.value
    val nextD = this.derivative + that.derivative
    Dual(nextV, nextD)
  }

  def -(that: Dual[A])(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = this.value - that.value
    val nextD = this.derivative - that.derivative
    Dual(nextV, nextD)
  }

  def pow(r: Double)(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = Math.pow(this.value, r)
    val nextD = r * Math.pow(this.value, r-1)
    Dual(nextV, nextD)
  }

  // Binary constant operations
  //
  // These are conveniences that could be implemented in terms of Dual.const

  def *(that: Double)(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = this.value * that
    val nextD = this.derivative * that
    Dual(nextV, nextD)
  }

  def /(that: Double)(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = this.value / that
    val nextD = this.derivative / that
    Dual(nextV, nextD)
  }

  def +(that: Double)(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = this.value + that
    val nextD = this.derivative
    Dual(nextV, nextD)
  }

  def -(that: Double)(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = this.value - that
    val nextD = this.derivative
    Dual(nextV, nextD)
  }

  // Unary operations
  //
  // Written in terms of `flatMap` to demonstrate it implements the chain rule.

  def sin(implicit ev: A =:= Double): Dual[Double] = {
    this.flatMap{ value =>
      val nextV = Math.sin(value)
      val nextD = Math.cos(value)
      Dual(nextV, nextD)
    }
  }

  def cos(implicit ev: A =:= Double): Dual[Double] = {
    this.flatMap{ value =>
      val nextV = Math.cos(value)
      val nextD = -Math.sin(value)
      Dual(nextV, nextD)
    }
  }

  def tan(implicit ev: A =:= Double): Dual[Double] = {
    this.flatMap{ value =>
      val nextV = Math.tan(value)
      val nextD = 1 + (nextV * nextV)
      Dual(nextV, nextD)
    }
  }

  def exp(implicit ev: A =:= Double): Dual[Double] = {
    this.flatMap{ value =>
      val nextV = Math.exp(this.value)
      val nextD = nextV
      Dual(nextV, nextD)
    }
  }
}
object Dual {
  def apply[A](value: A): Dual[A] = pure(value)

  def pure[A](value: A): Dual[A] =
    Dual(value, 1.0)

  /** Lift a constant value (i.e. one with derivative zero) to a Dual number. */
  def const[A](value: A): Dual[A] =
    Dual(value, 0.0)
}
