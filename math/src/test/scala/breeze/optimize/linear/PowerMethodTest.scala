package breeze.optimize.linear

import breeze.numerics.abs
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import breeze.linalg._

@RunWith(classOf[JUnitRunner])
class PowerMethodTest extends FunSuite {
  val n = 5
  val gram = new DenseMatrix[Double](n, n, Array(5.6880,-4.5286,6.7923,0.3049,-6.0388,
    -4.5286,8.0638,-6.9012,-2.6776,6.1795,
    6.7923,-6.9012,12.5510,-1.1917,-8.3500,
    0.3049,-2.6776,-1.1917,4.0684,-1.7535,
    -6.0388,6.1795,-8.3500,-1.7535,8.2831))
  val init = DenseVector(0.1770,0.2505,1.5957,0.7204,0.9246)
  val eigs = eigSym(gram)

  test("max eigen value from power method approximately equal to eigSym max") {
    val eigenGold = max(eigs.eigenvalues)
    val pm = new PowerMethod[DenseVector[Double], DenseMatrix[Double]]()
    val eigenApprox = pm.eigen(init, gram)
    assert(abs(eigenGold - eigenApprox) < 1e-3)
  }

  test("min eigen value from power method approximately equal to eigSym min") {
    val eigenGold = min(eigs.eigenvalues)
    val pm = new PowerMethod[DenseVector[Double], DenseMatrix[Double]]()
    val inverseGram = gram \ DenseMatrix.eye[Double](gram.rows)
    val eigenApprox = 1.0/pm.eigen(init, inverseGram)
    assert(abs(eigenGold - eigenApprox) < 1e-3)
  }

  test("min eigen value from inverse power method approximately equal to eigSym min") {
    val eigenGold = min(eigs.eigenvalues)
    val pmInv = new PowerMethod[DenseVector[Double], DenseMatrix[Double]](10, 1e-5, true)
    val R = cholesky(gram).t
    val eigenApprox = 1.0/pmInv.eigen(init, R)

    val inverseGram = gram \ DenseMatrix.eye[Double](gram.rows)
    val pm = new PowerMethod[DenseVector[Double], DenseMatrix[Double]]()
    val eigenApproxInv = 1.0/pm.eigen(init, inverseGram)
    assert(abs(eigenGold - eigenApprox) < 1e-3)
  }
}
