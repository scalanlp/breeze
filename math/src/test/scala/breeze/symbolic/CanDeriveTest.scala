package breeze.symbolic

import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
  * These tests check that for a function <code>f</code> and a point <code>x</code>
  * the following equations should hold for small <code>delta</code>s:
  * <code>f(x) - f(x - delta) =~ f'(x)<code>
  * <code>f(x + delta) - f(x) =~ f'(x)<code>
  *
  * The definitions of the functions <code>f</code> has to be crafted so that
  * we don't run into edge cases where the derivation is <code>NaN</code>.
  */
@RunWith(classOf[JUnitRunner])
class CanDeriveTest extends FunSuite with Checkers with Matchers {
  val complicatedFunction = Var() *:* Exponential(Var() - Const(1.0) +:+ Division(Const(3.0), Var() +:+ Const(50.0))) +:+ Var()

  test("derives constants") {
    testDerivation(Const(100.0))
  }

  test("derives variables") {
    testDerivation(Var())
  }

  test("derives complicated formulas") {
    testDerivation(complicatedFunction)
  }

  test("derives chains") {
    val func = Chain(complicatedFunction, Var() *:* Const(0.5) +:+ Const(15.0))
    testDerivation(func)
  }

  def testDerivation[SF <: SymbolicFunction[SF], D <: SymbolicFunction[D]](sf: SF)(
    implicit canCreateFunction: CanCreateFunction[Double, Double, SF],
    canDerive: CanDerive.Aux[SF, D],
    canCreateDerivationFunction: CanCreateFunction[Double, Double, D]
  ): Unit = {
    val func = canCreateFunction.createFunction(sf)
    val derivedSymbolicFunction = canDerive.derivation(sf)
    // Helpful for debugging
    // println(s"F  = $sf\nF' = $derivedSymbolicFunction")
    val derivedFunc = canCreateDerivationFunction.createFunction(derivedSymbolicFunction)
    (-20.0 to 20.0 by 0.5).foreach { x =>
      val value = func(x)
      val derivation = derivedFunc(x)
      // Derivation behaves linearly in close proximity
      val distance = 0.01
      val diffLeft = value - func(x - distance)
      val diffRight = func(x + distance) - value
      if (derivation != 0.0 &&
        !diffLeft.isInfinity && !diffRight.isInfinity &&
        !diffLeft.isNaN && !diffRight.isNaN
      ) {
        // Helpful for debugging:
        // println(s"For x = $x comparing $diffLeft <=> ${derivation * distance} <=> $diffRight")
        (derivation * distance) should be(diffLeft +- 0.1 * diffLeft.abs)
        (derivation * distance) should be(diffRight +- 0.1 * diffRight.abs)
      }
    }
  }
}
