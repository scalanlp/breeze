package breeze.linalg

import breeze.linalg.operators.OpMulMatrix
import breeze.linalg.support.CanTranspose
import breeze.optimize.{DiffFunction, GradientTester, LBFGS}
import breeze.stats.distributions.RandBasis
import org.scalatest.FunSuite

class LSMRTest extends FunSuite {

   test("simple dense solve") {
     val matrix = DenseMatrix(
       (1.0, 3.0, 4.0),
       (2.0, 0.0, 6.0)
     )
     val b = DenseVector(1.0, 3.0)
     val lsmrSolved = LSMR.solve(matrix, b)
     val solved : DenseVector[Double] = matrix \ b

     assert(norm(solved - lsmrSolved) < 1E-5, s"$solved $lsmrSolved")
   }

  test("regularized solve") {
    val matrix = DenseMatrix(
      (1.0, 3.0, 4.0),
      (2.0, 0.0, 6.0)
    )
    val b = DenseVector(1.0, 3.0)
    // checked against original implementation
    val solved = DenseVector(0.16937, -0.25754, 0.42227)
    val bfgsSolved = lbfgsSolve(matrix, b, 1)
    val lsmrSolved = LSMR.solve(matrix, b, regularization = 1.0)


    assert(norm(solved - lsmrSolved) < 1E-5, s"$solved $lsmrSolved")
    assert(norm(solved - bfgsSolved) < 1E-5, s"$solved $bfgsSolved")
  }

  test("regularized solve, 2.0") {
    val matrix = DenseMatrix(
      (1.0, 3.0, 4.0),
      (2.0, 0.0, 6.0)
    )
    val b = DenseVector(1.0, 3.0)
    val bfgsSolved = lbfgsSolve(matrix, b, 2.0)
    val lsmrSolved = LSMR.solve(matrix, b, regularization = 2.0)

    assert(norm(bfgsSolved - lsmrSolved) < 1E-5, s"$bfgsSolved $lsmrSolved")
  }

  def gen = RandBasis.mt0.uniform

  test("big regularized solve, 2.0") {
    val g = gen
    val matrix = DenseMatrix.rand(100, 100, g)
    val b = DenseVector.rand(100, g)
    val bfgsSolved = lbfgsSolve(matrix, b, 2.0)
    val lsmrSolved = LSMR.solve(matrix, b, regularization = 2.0, tolerance = 1E-9)

    assert(norm(bfgsSolved - lsmrSolved) < 1E-2, s"$bfgsSolved $lsmrSolved")
  }


  private def lbfgsSolve(mat: DenseMatrix[Double], target: DenseVector[Double], reg: Double = 0.0) = {
    val obj = new DiffFunction[DenseVector[Double]] {
      override def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val y = target - mat * x
        ( (y dot y) + (x dot x * reg), -mat.t * y * 2.0 + (x * (2 * reg)))
      }
    }
     GradientTester.test[Int, DenseVector[Double]](obj, DenseVector.rand[Double](mat.cols, gen), 1.0)

    new LBFGS[DenseVector[Double]](tolerance = 1E-9).minimize(obj, DenseVector.rand[Double](mat.cols, gen))
  }

  test("a few lsmr tests") {
    for(m <- 1 until 10; n <- 1 until 10) {
      lsmrTest(m, n)
    }

  }


  /**
   *
   * cribbed from lsmrtest.m
   *

   *
   * @param m
   * @param n
   */
  def lsmrTest(m: Int, n: Int) = {

    /*
    * This is a simple example for testing LSMR.
    * It uses the leading m*n submatrix from
    * A = [ 1
    *       1 2
    *         2 3
    *           3 4
    *             ...
    *               n ]
    * suitably padded by zeros.
    */
    case object A {

      implicit object mulADV extends OpMulMatrix.Impl2[A.type,
                                                       DenseVector[Double], DenseVector[Double]] {
        override def apply(v: A.type, v2: DenseVector[Double]): DenseVector[Double] = {
          assert(v2.length == n)
          val d = DenseVector.range(1, n + 1).map(_.toDouble)
          val y1 = (
            DenseVector.tabulate(n + 1)(i => if (i < n) v2(i) * d(i) else 0.0)
              + DenseVector.tabulate(n + 1)(i => if (i > 0) v2(i - 1) * d(i - 1) else 0.0)
            )

          if (m <= n + 1) {
            y1(0 until m)
          } else {
            DenseVector.vertcat(y1, DenseVector.zeros(m - n - 1))
          }

        }
      }

       implicit object mulATDV extends OpMulMatrix.Impl2[Transpose[A.type],
                                                         DenseVector[Double], DenseVector[Double]] {
        override def apply(v: Transpose[A.type], v2: DenseVector[Double]): DenseVector[Double] = {
          assert(v2.length == m)
          val d = DenseVector.range(1, m + 1).map(_.toDouble)
          val y1 = (
            (d :* v2)
              + DenseVector.tabulate(m)(i => if (i < m - 1) d(i) * v2(i + 1) else 0.0)
            )

          if (m >= n) {
            y1(0 until n)
          } else {
            DenseVector.vertcat(y1, DenseVector.zeros(n - m))
          }

        }
      }

      implicit object Trans extends CanTranspose[A.type, Transpose[A.type]] {
        override def apply(from: A.type): Transpose[A.type] = Transpose(from)
      }

    }

    val xtrue = DenseVector.range(n, 0, -1).map(_.toDouble)
    val b = A * xtrue
    val xsolve = LSMR.solve(A, b)
    val r = b - A * xsolve
    val normr = norm(r)
    assert(normr < 1E-4, normr)
  }

 }
