package fdl

/**
  * One-dimensional dual number, implementing forward-mode automatic differentiation.
  *
  * This implementation also defines a monad.
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
  // These could be written in terms of flatMap but it is slightly clearer to
  // present them directly, and this code is intended for learning.

  def sin(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = Math.sin(this.value)
    val nextD = Math.cos(this.value)
    Dual(nextV, nextD)
  }

  def cos(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = Math.cos(this.value)
    val nextD = -Math.sin(this.value)
    Dual(nextV, nextD)
  }

  def tan(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = Math.tan(this.value)
    val nextD = 1 + (nextV * nextV)
    Dual(nextV, nextD)
  }

  def exp(implicit ev: A =:= Double): Dual[Double] = {
    val nextV = Math.exp(this.value)
    val nextD = nextV
    Dual(nextV, nextD)
  }
}
object Dual {
  def pure[A](value: A): Dual[A] =
    Dual(value, 1.0)

  def const[A](value: A): Dual[A] =
    Dual(value, 0.0)
}
