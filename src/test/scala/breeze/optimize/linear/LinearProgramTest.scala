package breeze.optimize.linear

import org.scalatest.FunSuite
import breeze.linalg.DenseVector

/**
 * TODO
 *
 * @author dlwh
 **/
class LinearProgramTest extends FunSuite {

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


  test("dsl") {
    //    http://www.tu-chemnitz.de/mathematik/discrete/manuals/cplex/doc/getstart/html/cpxGSilocplex13.0html
    val lp = new LinearProgram()
    import lp._
    val x0 = Real()
    val x1 = Real()
    val x2 = Real()

    val lpp =  ( (x0 +  x1 * 2 + x2 * 3 )
      subjectTo ( x0 * -1 + x1 + x2 <= 20)
      subjectTo ( x0 - x1 * 3 + x2 <= 30)
      subjectTo ( x0 <= 40 )
      )

    val result = maximize( lpp)

    assert( (result.result - DenseVector(40.0,17.5,42.5)).norm(2) < 1E-4)


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

}
