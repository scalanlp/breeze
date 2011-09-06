package scalanlp.optimize

/**
 * 
 * @author dlwh
 */
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith;

import scalanlp.serialization.FileSerialization
import scalanlp.stats.distributions.Gaussian
import scalala.tensor.dense.DenseVector
import scalanlp.util.logging.ConsoleLogging
import scalala.library.Library._
;

@RunWith(classOf[JUnitRunner])
class ContrastiveDivergenceTest extends FunSuite with Checkers {
  test("simple test") {
        val data = (new Gaussian(3,1).samples take 1000).toSeq;
    def trans(mean: DenseVector[Double]) = { (x:Double) =>
      new Gaussian(mean(0),1)
    }
    def deriv(theta: DenseVector[Double]) = { (x:Double) => DenseVector(x-theta(0)) }
    val opt = new ContrastiveDivergenceOptimizer[Double,DenseVector[Double]](trans _ ,deriv _ ,0.01);

    val result = opt.maximize(data,DenseVector(-2.0));
    assert((result - 3.0).norm(1) < 1E1)

  }
}