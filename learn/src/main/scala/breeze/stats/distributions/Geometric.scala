package breeze.stats.distributions

import runtime.ScalaRunTime
import breeze.optimize.DiffFunction
import breeze.util._

/**
 * 
 * @author dlwh
 */

case class Geometric(p: Double)(implicit rand: RandBasis=Rand) extends DiscreteDistr[Int] with Moments[Double] {
  require(p >= 0)
  require(p <= 1)

  // from "Random Number Generation and Monte CArlo Methods"
  def draw() = math.ceil(math.log(rand.uniform.get) / math.log(1-p)).toInt


  def probabilityOf(x: Int) = math.pow((1-p),x) * p

  def mean = (1) / p

  def variance = (1-p) / p / p

  def mode = 0
  def entropy = (-(1 - p) * math.log(1-p) - p * math.log(p)) / p

  override def toString() = ScalaRunTime._toString(this)
}

object Geometric extends ExponentialFamily[Geometric,Int] with HasConjugatePrior[Geometric,Int] {
  type Parameter = Double
  case class SufficientStatistic(sum: Double, n: Double) extends breeze.stats.distributions.SufficientStatistic[SufficientStatistic] {
    def +(t: SufficientStatistic) = SufficientStatistic(sum + t.sum, n + t.n)

    def *(weight: Double) = SufficientStatistic(sum * weight, n * weight)
  }

  def emptySufficientStatistic = SufficientStatistic(0,0)

  def sufficientStatisticFor(t: Int) = SufficientStatistic(t,1)

  def mle(stats: SufficientStatistic) = stats.n / stats.sum

  def likelihoodFunction(stats: SufficientStatistic) = new DiffFunction[Geometric.Parameter] {
    def calculate(p: Geometric.Parameter) = {
      val obj = stats.n * math.log(p) + stats.sum * math.log(1-p)
      val grad = stats.n / p - stats.sum / (1-p)
      (-obj,-grad)

    }
  }

  def distribution(p: Geometric.Parameter) = new Geometric(p)

  type ConjugatePrior = Beta
  val conjugateFamily = Beta

  def predictive(parameter: conjugateFamily.Parameter) = TODO

  def posterior(prior: conjugateFamily.Parameter, evidence: TraversableOnce[Int]) = {
    evidence.foldLeft(prior) { (acc,x) =>
      (acc._1 + 1, acc._2 + x)
    }
  }
}