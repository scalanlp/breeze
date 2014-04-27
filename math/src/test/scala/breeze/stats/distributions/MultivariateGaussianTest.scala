package breeze.stats.distributions

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith
import breeze.numerics._
import breeze.linalg._
import breeze.stats._


@RunWith(classOf[JUnitRunner])
class MultivariateGaussianTest extends FunSuite with Checkers {
  import Arbitrary.arbitrary

  val N = 5

  implicit def genVector: Arbitrary[DenseVector[Double]] = {
    Arbitrary { Gen.wrap(DenseVector.rand(N)) }
  }

  implicit def genMatrix: Arbitrary[DenseMatrix[Double]] = {
    Arbitrary { Gen.wrap{
      // needs to be positive semidefinite
      val a = DenseMatrix.rand(N,N)/10.0
      a + a.t + DenseMatrix.eye[Double](N)
    }
    }
  }


    test("Probability of mean") {
      check( Prop.forAll { (m: DenseVector[Double], s: DenseMatrix[Double])=> {
        val b = new MultivariateGaussian(m,s)
        b.unnormalizedLogPdf(m) == 0.0
      }
      })
    }


    test("Probability of N(0,1)(1) propto exp(-.5))") {
      assert(new MultivariateGaussian(DenseVector(0.0),DenseMatrix.ones(1,1)).unnormalizedLogPdf(DenseVector(1.0)) === -0.5)
    }

    implicit def arbDistr = Arbitrary {
      for(mean <- genVector.arbitrary; std <- genMatrix.arbitrary) yield new MultivariateGaussian(mean,std);
    }

    val numSamples = 5000

    test("mean") {
      check(Prop.forAll { (distr: MultivariateGaussian)=>
        val sample = DenseVector.horzcat(distr.sample(numSamples):_*)
        val m = mean(sample(*, ::))
        if ( norm(m - distr.mean, Double.PositiveInfinity) > 1E-1) {
          println("MExpected " + distr.mean + " but got " + m)
          false
        } else {
          true
        }

      })
    }

    val VARIANCE_TOLERANCE = 0.1
    test("variance") {
      check(Prop.forAll { (distr: MultivariateGaussian)=>
      // try twice, and only fail if both fail.
      // just a little more robustness...
        Iterator.range(0, 2).exists{ _ =>
          val sample = DenseVector.horzcat(distr.sample(numSamples):_*)
          val vari = cov(sample.t)

          if(max(abs(vari - distr.variance)) > VARIANCE_TOLERANCE) {
            println("Expected " + distr.variance + " but got " + vari)
            false
          } else true
        }
      })
    }
  }
