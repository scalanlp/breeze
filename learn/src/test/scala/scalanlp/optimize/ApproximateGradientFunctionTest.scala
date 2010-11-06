package scalanlp.optimize

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.dense._;

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class ApproximateGradientFunctionTest extends FunSuite with Checkers {
  import Arbitrary._;

  implicit val arbVector : Arbitrary[Vector] = Arbitrary(for {
    n <- arbitrary[Int] suchThat { _ > 0 }
    d <- arbitrary[Double]
  } yield ( (rand(n.abs%40+1) * d value) : Vector));

  test("simple quadratic function") {
    val f = new DiffFunction[Int,Vector] {
      def calculate(x: Vector) = {
        val sqrtfx = norm((x - 3),2)
        val grad = (x-3) * 2 value;
        (sqrtfx * sqrtfx, grad);
      }
    }
    val approxF = new ApproximateGradientFunction[Int,Vector](f);

    check(Prop.forAll { (x:Vector) =>
      val ap = approxF.gradientAt(x)
      val tr = f.gradientAt(x);
      assert(norm(ap - tr, 2) < 1E-4 * norm(ap,2), ap.toString + " " + tr);
      true
    });

  }
}