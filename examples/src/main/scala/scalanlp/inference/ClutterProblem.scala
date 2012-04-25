package scalanlp.inference

import scalanlp.stats.distributions.{Bernoulli, Gaussian}
import scalanlp.util._


/**
 * Demonstrates ExpectationPropagation.
 *
 * The basic clutter problem, from Minka 2001
 *
 * Basically we have a simple mixture model of the form
 * p(x|mu)  = (1-w) N(x | mu, 1) + w N(x | 0, v_noise).
 * p(mu) = N(mu|0,v_mu)
 *
 * w is the proportion of noise, and v's are variances. These
 * are assumed known.
 *
 * The joint probability of the data x_i with the parameter mu is:
 *
 * P(x, mu) = p(mu) * \prod_i p(x_i | mu)
 *
 * The idea is the approximate p(mu | x) by q(mu), where
 *
 * q(mu) = N(mu | m, v)
 *
 * i.e. by a Gaussian.
 *
 *
 * XXXX
 *
 *
 * @author dlwh
 */
object ClutterProblem {
  def main(args: Array[String]) {
    // First, define the fixed parameters
    val w = 0.2
    val v_noise = 10
    val v_mu = 100

    // Next, define the approximate factor f_~
    // Note that this is just a gaussian with factor product and division
    case class ApproxFactor(m: Double, v: Double) extends ExpFactor[ApproxFactor] {
      def *(f: ApproxFactor) = if(f.v == Double.PositiveInfinity) this else {
        val vn = 1/(1/v + 1/f.v)
        val mn = (m * f.v + f.m * v)/(v + f.v)
        assert(!mn.isNaN, (this, "*", f))
        ApproxFactor(mn, vn)
      }

      def /(f: ApproxFactor) = if(f.v == Double.PositiveInfinity) this else {
        val vn = 1/(1/v - 1/f.v)
        val mn = m + vn/f.v * (m - f.m)
        if(mn == Double.NaN) ApproxFactor(0.0, Double.PositiveInfinity)
        else ApproxFactor(mn, vn)
//        if(vn <= 0 || vn.isInfinite) this
//        else {
//        }
      }

      def *(f: Double) = this

      def **(f: Double) = {
        copy(v=v/f)
      }

      def pdf(x: Double) = Gaussian(m,v).pdf(x)

      def logPartition = Gaussian(m,v).logNormalizer

      def isConvergedTo(f: ApproxFactor, diff: Double) = (
        scalanlp.util.closeTo(m, f.m, diff)
        && scalanlp.util.closeTo(v, f.v, diff)
      )
    }

    def project(q: ApproxFactor, point: Double, power: Double) = {
      val pdfNoise = Gaussian(0, v_noise).pdf(point)
      val Z_n = (1-w) * Gaussian(q.m, q.v + 1).pdf(point) + w * pdfNoise
      val r_n = 1 - w/Z_n * pdfNoise
      val m = q.m + r_n * q.v/(q.v+ 1) * (point - q.m)
      val v = (
        q.v
          - r_n * q.v * q.v/(q.v+ 1)
          + r_n * (1-r_n) * math.pow(q.v * (point - q.m)/(q.v + 1), 2)
        )

      assert(v >= 0, (v, power))

      ApproxFactor(m, v) -> math.log(Z_n)
    }

    // simulate some data
    val trueMu = 2
    val gen = for {
      notNoise <- new Bernoulli(1 - w)
      x <- Gaussian(I(notNoise) * trueMu, if(notNoise) 1 else v_noise)
    } yield x

    val data = gen.sample(1000)
    println(data)


    val init = data.map(ApproxFactor(_,100000))
    val ep = new ConvexEP({project _}, 1E-8)
    for( state <- ep.inference(ApproxFactor(0.0, v_mu),data,init).take(50)) {
      println(state.logPartition, state.q)
      assert(!state.logPartition.isNaN, state.toString)
    }


  }

}
