package breeze.optimize.linear

import org.scalatest.funsuite.AnyFunSuite
import breeze.linalg.{norm, DenseVector}

/**
 * TODO
 *
 * @author dlwh
 **/
class LinearProgramTest extends AnyFunSuite {

  /*
  test("ILP Test") {
    // http://www.cs.ucf.edu/~shzhang/Combio/lec16.pdf

    val lp = new LinearProgram
    import lp._
    val x1,x2 = Integer()
    val result = maximize (
      (x1 * 355.0 + x2 * 300.0)
      subjectTo(x1 + x2 <= 200.5, x1 * 9 + x2 * 6 <= 1566, x1 * 12 + x2 * 16 <= 2880, x1 * -1 <= 0, x2 * -1 <= 0)
    )

    assert(result.valueOf(x1) === 122)
    assert(result.valueOf(x2) === 78)
  }*/

  test("maximize") {
    //    http://www.tu-chemnitz.de/mathematik/discrete/manuals/cplex/doc/getstart/html/cpxGSilocplex13.0html
    val lp = new LinearProgram()
    import lp._
    val x0 = Real()
    val x1 = Real()
    val x2 = Real()

    val lpp = (x0 + x1 * 2 + x2 * 3)
      .subjectTo(x0 * -1 + x1 + x2 <= 20)
      .subjectTo(x0 - x1 * 3 + x2 <= 30)
      .subjectTo(x0 <= 40)

    val result = maximize(lpp)

    assert(norm(result.result - DenseVector(40.0, 17.5, 42.5), 2) < 1E-4)
  }

  test("minimize") {
    val lp = new LinearProgram
    import lp._
    val x0 = Real()
    val x1 = Real()
    val x2 = Real()
    val x3 = Real()

    val lpp = (x0 * 0.5 + x1 * 3 + x2 + x3 * 4)
      .subjectTo(x0 + x1 + x2 + x3 <= 40)
      .subjectTo(x0 * 2 + x1 - x2 - x3 >= 10)
      .subjectTo(x3 - x1 >= 10)
      .subjectTo(List(x0, x1, x2, x3).map(x => x >= 0): _*)

    val res = minimize(lpp)
    assert(norm(res.result - DenseVector(10.0, 0.0, 0.0, 10.0), 2) < 1E-4)
  }

  /*
  test("Binary ILP Test") {
    // http://www.cs.ucf.edu/~shzhang/Combio/lec16.pdf

    val lp = new LinearProgram
    import lp._
    val x1,x2 = Binary()
    val result = maximize (
      (x1 * 355.0 + x2 * 300.0)
        subjectTo(x1 + x2 <= 200.5, x1 * 9 + x2 * 6 <= 1566, x1 * 12 + x2 * 16 <= 2880, x1 * -1 <= 0, x2 * -1 <= 0)
    )

    assert(result.valueOf(x1) === 1)
    assert(result.valueOf(x2) === 1)
  }
 */

  test("maximize with solve method") {
    val lp = new LinearProgram
    import lp._
    val x0 = Real()
    val x1 = Real()

    val max = maximize(x0 + x1)
      .subjectTo(x0  <= 20)
      .subjectTo(x1 <= 30)

    val res = max.solve
    assert(norm(res.result - DenseVector(20.0, 30.0), 2) < 1E-4)

    assertThrows[AssertionError](minimize(max))

  }

  test("minimize with solve method") {
    val lp = new LinearProgram
    import lp._
    val x0 = Real()
    val x1 = Real()

    val min = minimize(x0 + x1)
      .subjectTo(x0  >= 20)
      .subjectTo(x1 >= 30)

    val res = min.solve
    assert(norm(res.result - DenseVector(20.0, 30.0), 2) < 1E-4)

    assertThrows[AssertionError](maximize(min))

  }

}
