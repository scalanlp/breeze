package scalanlp.optimize

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

import scalala.tensor.dense._;
import scalala.library.Library.norm;

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class ApproximateGradientFunctionTest extends FunSuite with Checkers {
  import Arbitrary._;

  implicit val arbVector : Arbitrary[DenseVector[Double]] = Arbitrary(for {
    n <- arbitrary[Int] suchThat { _ > 0 }
    d <- arbitrary[Double]
  } yield ( (DenseVector.rand(n.abs%40+1) * d) : DenseVector[Double]));

  test("simple quadratic function") {
    val f = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]) = {
        val sqrtfx = norm((x - 3),2)
        val grad = (x-3) * 2;
        (sqrtfx * sqrtfx, grad);
      }
    }
    val approxF = new ApproximateGradientFunction[Int,DenseVector[Double]](f);

    check(Prop.forAll { (x:DenseVector[Double]) =>
      val ap = approxF.gradientAt(x)
      val tr = f.gradientAt(x);
      assert(norm(ap - tr, 2) < 1E-4 * norm(ap,2), ap.toString + " " + tr);
      true
    });

  }
}