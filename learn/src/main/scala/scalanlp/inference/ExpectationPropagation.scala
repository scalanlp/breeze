package scalanlp.inference

import math._
import scalala.library.{Numerics,Library}
import Numerics._
import runtime.ScalaRunTime
import scalanlp.util._
import scalala.tensor.dense._
import scalanlp.stats.distributions.{Dirichlet, Bernoulli, Gaussian}

/**
 * 
 * @author dlwh
 */
class ExpectationPropagation[F,Q](project: (Q,F)=>(Q,Double))
                                 (implicit qFactor: Q <:<FactorLike[Q],
                                  qProd: FactorProduct[Q,Q,Q],
                                  qDiv: FactorQuotient[Q,Q,Q]) {
  case class State(f_~ : IndexedSeq[Q], q: Q, prior: Q, partitions: IndexedSeq[Double]) {
    def logPartition = f_~.foldLeft(prior)(_*_).logPartition + partitions.sum
  }

  def inference(prior: Q, f: IndexedSeq[F], initialF_~ : IndexedSeq[Q]):Iterator[State] = {
    val initQ: Q = initialF_~.foldLeft(prior)(_ * _)

    val initPartitions = IndexedSeq.fill(f.length)(Double.NegativeInfinity)

    // pass through the data
    Iterator.iterate(State(initialF_~, initQ * (-initQ.logPartition), prior, initPartitions)) { state =>
      (0 until f.length).iterator.foldLeft(state) { (state,i) =>
        val State(f_~, q, _, partitions) = state
        val fi = f(i)
        val fi_~ = f_~(i)
        val q_\  = q / fi_~
        val (new_q, new_partition) = project(q_\ , fi)
        val newF_~ = f_~.updated(i,new_q / q_\)
        State(newF_~, new_q, prior, partitions.updated(i, new_partition))
      }
    } drop(1)
  }



}

object ExpectationPropagation extends App {
  val prop = 0.5
  val mean = 2
  val gen = for {
    a <- new Bernoulli(prop)
    x <- Gaussian(I(a) * mean,3)
  } yield x

  val data = gen.sample(1000)

  case class ApproxTerm(s: Double = 0.0, b: DenseVector[Double] = DenseVector.zeros(2)) extends FactorLike[ApproxTerm] {
    def logPartition = s + Numerics.lbeta(b)

    def *(f: Double) = copy(s = s + f)
  }

  implicit object QProduct extends FactorProduct[ApproxTerm,ApproxTerm, ApproxTerm] with FactorQuotient[ApproxTerm,ApproxTerm,ApproxTerm] {
    def product(f1: ApproxTerm, f2: ApproxTerm) = {
      ApproxTerm(f1.s + f2.s, f1.b + f2.b)
    }

    def quotient(f1: ApproxTerm, f2: ApproxTerm) = {
      ApproxTerm(f1.s - f2.s, f1.b - f2.b)
    }
  }

  def likelihood(x: Double):DenseVectorCol[Double] = {
    DenseVector(Gaussian(0,3).pdf(x), Gaussian(mean,3).pdf(x))
  }

  def solve(old: DenseVector[Double], target: DenseVector[Double]) = {
    val guess = old + 0.0;
    for(i <- 0 until 20) {
      val t2 = target + digamma(guess.sum)
      for(i <- 0 until 5) {
        guess -= ((guess.map(digamma _ ) - t2) :/ (( (guess + 1E-4).map(digamma _) - guess.map(digamma _) )/1E-4))
      }
    }
    guess
  }


  def project(q: ApproxTerm, x: Double): (ApproxTerm, Double) = {
    val like = likelihood(x)
    val target = q.b.map(Numerics.digamma) - Numerics.digamma(q.b.sum) +  (like / (like dot q.b)) - 1/q.b.sum
    val normalizer = likelihood(x) dot Library.normalize(q.b, 1)
    val mle = solve(q.b, target)
    assert(!normalizer.isNaN,(mle,q.b,like,Library.normalize(q.b,1)))

    ApproxTerm(-lbeta(mle+1), mle) -> math.log(normalizer)
  }

  val ep = new ExpectationPropagation(project _)
  for( state <- ep.inference(ApproxTerm(0.0,DenseVector.ones(2)), data, Array.fill(data.length)(ApproxTerm())) take 20) {
    println(state.logPartition, state.q)
    assert(!state.logPartition.isNaN, state.q.s + " " + state.q.b)
  }



}
