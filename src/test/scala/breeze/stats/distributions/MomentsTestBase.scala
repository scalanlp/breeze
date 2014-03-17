package breeze.stats
package distributions

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before
import org.scalatest.prop._
import org.scalacheck._
import java.util.zip.DeflaterInputStream

/**
 * @author dlwh
 */
trait MomentsTestBase[T] extends FunSuite with Checkers {
  implicit def arbDistr: Arbitrary[Density[T] with Rand[T] with Moments[Double, Double]];
  val numSamples = 10000;
  def asDouble(x: T):Double
  def fromDouble(x: Double):T

  def numFailures: Int = 2

  test("mean") {
    check(Prop.forAll { (distr: Density[T] with Rand[T] with Moments[Double, Double])=>
       val sample = distr.sample(numSamples).map(asDouble _)
       val m = mean(sample)
       if ( (m - distr.mean).abs/(m.abs max 1) > 1E-1) {
         println("MExpected " + distr.mean + " but got " + m)
         false
       } else {
         true
       }

    })
  }

  val VARIANCE_TOLERANCE = 5E-2
  test("variance") {
    check(Prop.forAll { (distr: Density[T] with Rand[T] with Moments[Double, Double])=>
    // try twice, and only fail if both fail.
    // just a little more robustness...
      Iterator.range(0,numFailures).exists{ _ =>
        val sample = distr.sample(numSamples).map(asDouble _)
        val vari = variance(sample)

        if((vari - distr.variance).abs/(vari max 1) > VARIANCE_TOLERANCE) {
          println("Expected " + distr.variance + " but got " + vari)
          false
        } else true
      }
    })
  }


  test("mode") {
    check(Prop.forAll { (distr: Rand[T] with Density[T] with Moments[Double, Double])=>
      val sample = distr.sample(40)
      val probMode = distr(fromDouble(distr.mode))
//      if(distr.isInstanceOf[Poisson])
//        println(distr,probMode,sample.map{ distr },sample)
      sample.find(x => probMode < distr(x) - 1E-4) match {
        case Some(x) => println(s"$x has higher prob than mode ${distr.mode}"); false
        case None => true
      }
    })
  }

}
