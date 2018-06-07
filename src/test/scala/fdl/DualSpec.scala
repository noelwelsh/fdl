package fdl

import org.scalacheck._
import org.scalacheck.Prop._

object DualSpec extends Properties("Dual") {
  import Generators._

  property("evaluates correctly") =
    forAll(Generators.genExpr){ expr =>
      val dualV = exprToDual(expr).value
      val exprV = eval(expr)
      ((dualV.isNaN && exprV.isNaN) || (dualV ?= exprV)) :| s"Expr: ${print(expr)}"
    }
}
