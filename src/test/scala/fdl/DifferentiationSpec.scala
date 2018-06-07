package fdl

import org.scalacheck._
import org.scalacheck.Prop._

object DifferentiationSpec extends Properties("Differentiation") {
  import Generators._

  property("dual and reverse mode calculate the same result") =
    forAll(Generators.genDualAndReverse){ case (dual, reverse) =>
      (dual.value ?~= reverse.value)
    }

  property("dual and reverse mode calculate all derivative") =
    forAll(Generators.genDualAndReverse){ case (dual, reverse) =>
      (dual.derivative ?~= reverse.derivative)
    }

  property("dual and reverse mode arithmetic derivative") =
    forAll(Generators.genArithmetic(6)){ expr =>
      val dualD = exprToDual(expr).derivative
      val reverseD = exprToReverse(expr).derivative

      (dualD ?~= reverseD) :| s"Expr: ${print(expr)}"
    }

  property("dual and reverse mode trig derivative") =
    forAll(Generators.genTrig(6)){ expr =>
      val dualD = exprToDual(expr).derivative
      val reverseD = exprToReverse(expr).derivative

      (dualD ?~= reverseD) :| s"Expr: ${print(expr)}"
    }
}
