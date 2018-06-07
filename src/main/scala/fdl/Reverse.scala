package fdl

/** One-dimensional reverse-mode automatic differentation. */
final case class Reverse[A](value: A, cont: Double => Double) {
  def derivative: Double =
    this.cont(1.0)

  /** flatMap is the chain rule for reverse-mode automatic differentation. */
  def flatMap[B](f: A => Reverse[B]): Reverse[B] = {
    val self = this
    val next = f(value)
    Reverse(next.value, (d: Double) => self.cont(next.cont(1) * d))
  }

  // Binary operations

  def *(that: Reverse[Double])(implicit ev: A =:= Double): Reverse[Double] =
    Reverse(this.value * that.value, (d: Double) => (this.cont(d * that.value)) + (that.cont(d * this.value)))

  def /(that: Reverse[Double])(implicit ev: A =:= Double): Reverse[Double] =
    Reverse(this.value / that.value, (d: Double) => ((this.cont(d * that.value)) - (that.cont(d * this.value))) / (that.value * that.value))

  def +(that: Reverse[Double])(implicit ev: A =:= Double): Reverse[Double] =
    Reverse(this.value + that.value, (d: Double) => this.cont(d) + that.cont(d))

  def -(that: Reverse[Double])(implicit ev: A =:= Double): Reverse[Double] =
    Reverse(this.value - that.value, (d: Double) => this.cont(d) - that.cont(d))

  def pow(r: Double)(implicit ev: A =:= Double): Reverse[Double] =
    this.flatMap(x => Reverse(Math.pow(x, r), (_) => r * Math.pow(x, r - 1)))

  // Unary operations

  def sin(implicit ev: A =:= Double): Reverse[Double] =
    this.flatMap(x => Reverse(Math.sin(x), (_) => Math.cos(x)))

  def cos(implicit ev: A =:= Double): Reverse[Double] =
    this.flatMap(x => Reverse(Math.cos(x), (_) => -Math.sin(x)))

  def tan(implicit ev: A =:= Double): Reverse[Double] =
    this.flatMap(x => Reverse(Math.tan(x), (_) => 1 + (Math.tan(x) * Math.tan(x))))

  def exp(implicit ev: A =:= Double): Reverse[Double] =
    this.flatMap(x => Reverse(Math.exp(x), (_) => Math.exp(x)))
}
object Reverse {
  def apply[A](value: A): Reverse[A] =
    Reverse.pure(value)

  def pure[A](value: A): Reverse[A] =
    Reverse(value, d => d)

  def const[A](value: A): Reverse[A] =
    Reverse(value, d => 0)
}
