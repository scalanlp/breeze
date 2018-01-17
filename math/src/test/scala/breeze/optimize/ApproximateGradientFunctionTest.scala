package breeze.optimize

import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.scalacheck._
import org.junit.runner.RunWith

import breeze.linalg._

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class ApproximateGradientFunctionTest extends OptimizeTestBase {

  test("simple quadratic function") {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val sqrtfx = norm((x - 3.0), 2)
        val grad = (x - 3.0) * 2.0
        (sqrtfx * sqrtfx, grad)
      }
    }
    val approxF = new ApproximateGradientFunction[Int, DenseVector[Double]](f)

    check(Prop.forAll { (x: DenseVector[Double]) =>
      val ap = approxF.gradientAt(x)
      val tr = f.gradientAt(x)
      assert(norm(ap - tr, 2) < 1E-4 * math.max(norm(ap, 2), 1), ap.toString + " " + tr)
      true
    })

  }
}
