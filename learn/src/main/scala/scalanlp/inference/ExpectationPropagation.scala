package scalanlp.inference

import math._
import scalala.library.Numerics
import scalanlp.stats.distributions.{Bernoulli, Gaussian}
import runtime.ScalaRunTime

/**
 * 
 * @author dlwh
 */
class ExpectationPropagation[F,Q](project: (Q,F)=>(Q,Double))
                                 (implicit qFactor: Q <:<FactorLike[Q],
                                  qProd: FactorProduct[Q,Q,Q],
                                  qDiv: FactorQuotient[Q,Q,Q]) {
  case class State(f_~ : IndexedSeq[Q], q: Q) {
    def logPartition = f_~.reduceLeft(_*_).logPartition
  }

  def inference(prior: Q, f: IndexedSeq[F], initialF_~ : IndexedSeq[Q]):Iterator[State] = {
    val initQ: Q = initialF_~.foldLeft(prior)(_ * _)

    // pass through the data
    Iterator.iterate(State(initialF_~, initQ * (1/initQ.logPartition))) { state =>
      (0 until f.length).iterator.foldLeft(state) { (state,i) =>
        val State(f_~, q) = state
        val fi = f(i)
        val fi_~ = f_~(i)
        val q_\  = q / fi_~
        val (new_q, new_partition) = project(q_\ , fi)
        val newF_~ = f_~.updated(i,new_q / q_\ * new_partition)
        State(newF_~, new_q)
      }
    }
  }



}

object ExpectationPropagation extends App {
  val a = 10. // clutter variance
  val b = 10. // prior on theta
  val w = 0.4 // clutter percentage
  sealed trait ClutterFactor extends Factor[Double] with FactorLike[ClutterFactor];

  case class LikelihoodTerm(point: Double, scale:Double=0.0) extends ClutterFactor {
    def *(f: Double) = copy(point,scale + f)

    def logPartition = Double.PositiveInfinity

    def apply(theta: Double) = Numerics.logSum(log(1-w) + new Gaussian(theta,1).logPdf(point),
      log(w) + new Gaussian(0,a).logPdf(point))
  }

  case class ApproxFactor(mean: Double, variance: Double, scale: Double) extends ClutterFactor with FactorLike[ApproxFactor] {
    def *(f: Double) = copy(scale=scale + f)

    def logPartition = -scale

    def apply(theta: Double) = new Gaussian(mean,variance).unnormalizedLogPdf(theta) + scale

    override def toString() = ScalaRunTime._toString(this)
  }

  implicit val qProd = new FactorProduct[ApproxFactor,ApproxFactor,ApproxFactor] with FactorQuotient[ApproxFactor,ApproxFactor,ApproxFactor] {
    def product(f1: ApproxFactor, f2: ApproxFactor) = if(f2.variance == Double.PositiveInfinity) f1 else {
      val newMean = (f1.mean * f2.variance + f2.mean * f1.variance)/(f1.variance + f2.variance)
      val newVariance = (f1.variance * f2.variance / (f1.variance + f2.variance))
      ApproxFactor(newMean,newVariance,f1.scale + f2.scale)
    }

    def quotient(f1: ApproxFactor, f2: ApproxFactor) = if(f2.variance == Double.PositiveInfinity) f1 else {
      val newVariance = 1/(1/f1.variance - 1/f2.variance)
      val newMean = (f1.mean + newVariance / f2.variance * (f1.mean - f2.mean))
      ApproxFactor(newMean,newVariance,f1.scale - f2.scale)
    }
  }

  def project(q: ApproxFactor, f: ClutterFactor) = f match {
    case f @ LikelihoodTerm(point,scale) =>
      val z = Numerics.logSum(log(1-w) + new Gaussian(q.mean,(1 + q.variance)).logPdf(point), log(w) + new Gaussian(0,a).logPdf(point))
      val rho = 1-w * math.exp(new Gaussian(0,a).logPdf(point)-z)
      val vn = q.variance - rho * q.variance * q.variance / (q.variance + 1) + rho * (1 - rho) *math.pow(q.variance * (point - q.mean)/(q.variance + 1),2)
      val m = q.mean + rho * q.variance/(q.variance + 1) * rho * (point - q.mean)
      assert(!m.isNaN,(q.mean,q.variance))
      val qnew = new ApproxFactor(m,vn,-new Gaussian(m,vn).logNormalizer)
      (qnew,z)
    case f: ApproxFactor =>
      (q * f,0.0)
  }

  val mean = new Gaussian(0,b).draw()
  println(mean)

  val drawPoint = for {
    isClutter <- new Bernoulli(w)
    gaussian = if(isClutter) new Gaussian(0, a) else new Gaussian(mean,1)
    draw <- gaussian
  } yield {
    draw
  }

  val points = drawPoint.sample(1000)
  val factors = points.map(new LikelihoodTerm(_))
  val prior = new ApproxFactor(0,b,math.log(2 * math.Pi* b) * -.5)
  val initialFactors = Array.fill(factors.length)(new ApproxFactor(0,Double.PositiveInfinity,0))

  val ep = new ExpectationPropagation(project _).inference(prior,factors, initialFactors)
  for( state <- ep.take(100)) {
    println(state.q.mean,mean)
  }


}