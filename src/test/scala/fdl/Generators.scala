package fdl

import cats.implicits._
import org.scalacheck._
import org.scalacheck.cats.implicits._

object Generators {
  sealed trait Expr
  final case class Const(value: Double) extends Expr
  final case class Var(value: Double) extends Expr
  final case class Mul(left: Expr, right: Expr) extends Expr
  final case class Div(left: Expr, right: Expr) extends Expr
  final case class Add(left: Expr, right: Expr) extends Expr
  final case class Sub(left: Expr, right: Expr) extends Expr
  final case class Pow(expr: Expr, power: Double) extends Expr
  final case class Sin(expr: Expr) extends Expr
  final case class Cos(expr: Expr) extends Expr
  final case class Tan(expr: Expr) extends Expr
  final case class Exp(expr: Expr) extends Expr

  val genExpr: Gen[Expr] = {
    def iter(depth: Int): Gen[Expr] =
      if(depth == 0) Gen.choose(-10.0, 10.0).flatMap(d => Gen.oneOf(Var(d), Const(d)))
      else {
        Gen.oneOf(
          Gen.delay((iter(depth - 1), iter(depth - 1)).mapN(Mul(_, _))),
          Gen.delay((iter(depth - 1), iter(depth - 1)).mapN(Div(_, _))),
          Gen.delay((iter(depth - 1), iter(depth - 1)).mapN(Add(_, _))),
          Gen.delay((iter(depth - 1), iter(depth - 1)).mapN(Sub(_, _))),
          Gen.delay((iter(depth - 1 ), Gen.choose(-3, 3)).mapN((e, r) => Pow(e, r.toDouble))),
          Gen.delay(iter(depth - 1).map(Sin(_))),
          Gen.delay(iter(depth - 1).map(Cos(_))),
          Gen.delay(iter(depth - 1).map(Tan(_))),
          Gen.delay(iter(depth - 1).map(Exp(_)))
        )
      }

    val expr = Gen.choose(1, 8).flatMap(iter _)
    expr
  }


  def exprToDual(expr: Expr): Dual[Double] =
    expr match {
      case Var(value) => Dual(value)
      case Const(value) => Dual.const(value)
      case Pow(expr, power) => exprToDual(expr).pow(power)
      case Tan(expr) => exprToDual(expr).tan
      case Exp(expr) => exprToDual(expr).exp
      case Cos(expr) => exprToDual(expr).cos
      case Sin(expr) => exprToDual(expr).sin
      case Add(left, right) => exprToDual(left) + exprToDual(right)
      case Mul(left, right) => exprToDual(left) * exprToDual(right)
      case Sub(left, right) => exprToDual(left) - exprToDual(right)
      case Div(left, right) => exprToDual(left) / exprToDual(right)
    }

  def exprToReverse(expr: Expr): Reverse[Double] =
    expr match {
      case Var(value) => Reverse(value)
      case Const(value) => Reverse.const(value)
      case Pow(expr, power) => exprToReverse(expr).pow(power)
      case Tan(expr) => exprToReverse(expr).tan
      case Exp(expr) => exprToReverse(expr).exp
      case Cos(expr) => exprToReverse(expr).cos
      case Sin(expr) => exprToReverse(expr).sin
      case Add(left, right) => exprToReverse(left) + exprToReverse(right)
      case Mul(left, right) => exprToReverse(left) * exprToReverse(right)
      case Sub(left, right) => exprToReverse(left) - exprToReverse(right)
      case Div(left, right) => exprToReverse(left) / exprToReverse(right)
    }

  val genDual: Gen[Dual[Double]] = genExpr.map(e => exprToDual(e))
  val genReverse: Gen[Reverse[Double]] = genExpr.map(e => exprToReverse(e))

  val genDualAndReverse: Gen[(Dual[Double], Reverse[Double])] =
    genExpr.flatMap(e => Gen.const((exprToDual(e), exprToReverse(e))) :| s"Expr: $e")
}
