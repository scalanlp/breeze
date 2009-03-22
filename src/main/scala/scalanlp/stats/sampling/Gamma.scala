package scalanlp.stats.sampling;

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

import scalanlp.math.Numerics;
import Numerics._;
import Math._;


/**
 * Represents a Gamma distribution.
 * E[X] = shape * scale
 *
 * @author dlwh
 */
class Gamma(val shape : Double, val scale : Double) extends ContinuousDistr[Double] {
  if(shape <= 0.0 || scale <= 0.0)
    throw new IllegalArgumentException("Shape and scale must be positive");

  def pdf(x : Double) = Math.exp(logPdf(x));

  override def logPdf(x : Double) = {
    unnormalizedLogPdf(x) + logNormalizer;
  }
  
  val logNormalizer = - Numerics.lgamma(shape) - shape * Math.log(scale);

  override def unnormalizedLogPdf(x : Double) = (shape - 1) * Math.log(x) - x/scale;

  // Copied from Teh
  def draw() : Double = { 
    var aa = 0.0;
    var bb = 0.0;
    var cc = 0.0;
    var dd = 0.0;
    var uu = 0.0;
    var vv = 0.0;
    var ww = 0.0;
    var xx = 0.0;
    var yy = 0.0;
    var zz = 0.0;
    if (shape == 1.0) {
      /* Exponential */
      return scale * -Math.log(Rand.uniform.get);
    } else if (shape < 1.0) {
      /* Use Johnks generator */
      cc = 1.0 / shape;
      dd = 1.0 / (1.0 - shape);
      while (true) {
        xx = Math.pow(Rand.uniform.get(), cc);
        yy = xx + Math.pow(Rand.uniform.get(), dd);
        if (yy <= 1.0) {
          return scale * -Math.log(Rand.uniform.get()) * xx / yy;
        }
      }
      0.0; // silly;
    } else { /* shape > 1.0 */
      /* Use bests algorithm */
      bb = shape - 1.0;
      cc = 3.0 * shape - 0.75;
      while (true) {
        uu = Rand.uniform.get();
        vv = Rand.uniform.get();
        ww = uu * (1.0 - uu);
        yy = Math.sqrt(cc / ww) * (uu - 0.5);
        xx = bb + yy;
        if (xx >= 0) {
          zz = 64.0 * ww * ww * ww * vv * vv;
          if ((zz <= (1.0 - 2.0 * yy * yy / xx))
              || (Math.log(zz) <= 2.0 * (bb * Math.log(xx / bb) - yy))) {
            return xx * scale;
          }
        }
      }
      0.0; // silly;
    }
  }

}

object Gamma {
  type PoissonPosterior = Gamma with PoissonPrior;
  trait PoissonPrior extends ConjugatePrior[Double,Int] { self: Gamma =>
    // negative binomial distribution
    def predictive() = new DiscreteDistr[Int] {
      def draw() = {
        new Poisson(self.get).get
      }

      def probabilityOf(k :Int) = Math.exp(logProbabilityOf(k));
      private val p = 1. / (1. + scale);
      override def logProbabilityOf(k:Int) = {
        import math.Numerics._;
        import Math._;
        lgamma(shape + k) - lgamma(k) - lgamma(shape) + shape * log(p) + k * log(1-p);
      }
    }

    def posterior(ev: Iterator[(Int,Int)]) : PoissonPosterior = {
      val (tot,count) = ev.foldLeft( (0.0,0.0) ) { (tup,d) =>
        (tup._1 + d._1 * d._2, tup._2 +  d._2)
      }
      new Gamma(shape + tot, 1/(1/scale + count)) with PoissonPrior;
    }

    override def posterior(ev:Iterable[(Int,Int)]) : PoissonPosterior = posterior(ev.elements);
  }
}
