package fdl

import org.scalacheck._
import org.scalacheck.Prop._

object DifferentiationSpec extends Properties("Differentiation") {
  property("dual and reverse mode calculate the same result") =
    forAll(Generators.genDualAndReverse){ case (dual, reverse) =>
      (dual.value.isNaN && reverse.value.isNaN) ||
      (dual.value ?= reverse.value)
    }

  property("dual and reverse mode calculate the same derivative") =
    forAll(Generators.genDualAndReverse){ case (dual, reverse) =>
      (dual.derivative.isNaN && reverse.derivative.isNaN) ||
      (dual.derivative ?= reverse.derivative)
    }
}
