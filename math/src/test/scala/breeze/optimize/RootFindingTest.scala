package breeze.optimize

import org.scalatest.FunSuite
import breeze.numerics.closeTo


/**
  * Tests for the RootFinding object
  * @author abertout
  **/
class RootFindingTest extends FunSuite {
  val f = (x: Double) => x*x
  val f2 = (x: Double) => math.sin(x)
  val f2d = (x: Double) => math.cos(x)
  val f3 = (x: Double) => (x + 3)*(x - 1)*(x - 1)
  val f3d = (x: Double) => 3*x*x + 2*x - 5
  val f4 = (x: Double) => 2*x


  test("root finding general method") {
    val  found = RootFinding.find(f3,x0 = -4,x1 = Some(4/3d))
    assert(closeTo(found,1d) || closeTo(found,-3d))
    assert(closeTo(RootFinding.find(f2, 3),3.1416))
  }

  test("incorrect initial interval") {
    intercept[Exception] {
      RootFinding.brent(f4, -5, -2)
      RootFinding.find(f, 0.1)
    }
  }

  test("Brent's method") {
    val  found = RootFinding.brent(f3,x0 = -4,x1 = 4/3d)
    assert(closeTo(found,1d) || closeTo(found,-3d))
    assert(closeTo(RootFinding.brent(f2, 3,3.7),3.1416))
    //assert(closeTo(RootFinding.brent(f, -1,1),0))
  }

  test("bisection method"){
    val  found = RootFinding.bisection(f3,-4, 4/3d)
    assert(closeTo(found,1d) || closeTo(found,-3d))
    assert(closeTo(RootFinding.bisection(f2, 3, 4),3.1416))
  }

  test("Newton-Raphson's method"){
    val  found = RootFinding.newtonRaphson(f3,f3d, -4)
    assert(closeTo(found,1d) || closeTo(found,-3d))
    assert(closeTo(RootFinding.newtonRaphson(f2,f2d, 3, maxIter = 20),3.1416))
  }

  test("secant method"){
    val  found = RootFinding.secant(f3,5, -4)
    assert(closeTo(found,1d) || closeTo(found,-3d))
    assert(!closeTo(RootFinding.secant(f2,0, 3, maxIter = 20),3.1416)) //Divergence
    assert(closeTo(RootFinding.secant(f2,4, 3),3.1416))
  }


}
