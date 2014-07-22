package breeze.stats.mcmc

import breeze.benchmark._
import breeze.stats.distributions._
import breeze.stats.mcmc._

import spire.implicits.cfor

object MetropolisHastingsRunner extends MyRunner(classOf[MetropolisHastingsBenchmark])

class MetropolisHastingsBenchmark extends BreezeBenchmark {

  val burnIn = 1024*1024
  val dropCount = 25
  val numSamples = 1024*1024
  val bufferSize = 1024*32

  val epsilon = 1e-8
  def likelihood(x: Double) = 2*math.log1p(1+epsilon-x) + 3*math.log1p(x*x*x+epsilon) //Epsilon is present to avoid throwing exceptions in the unlikely event either 0 or 1 is sampled

  def pullAllSamples(m: Rand[Double]) = {
    var result = 0.0
    cfor(0)(i => i<numSamples, i => i+1)(i => {
      result = m.draw()
    })
    result
  }

  def pullAllSamplesWithWork(m: Rand[Double]) = {
    var result = 0.0
    cfor(0)(i => i<numSamples/4, i => i+1)(i => {
      val x = m.draw()
      cfor(0)(j => j < 400, j => j+1)(j => {
        result += math.log(math.exp(x)) / (1+x*x)
      })
    })
    result
  }

  def timeMarkovChain(reps: Int) = run(reps) {
    val m = MarkovChain.metropolisHastings(0.5, (_:Double) => Uniform(0,1))(likelihood _)
    pullAllSamples(m)
  }

  def timeMarkovChainEquiv(reps: Int) = run(reps) {
    val m = ArbitraryMetropolisHastings(likelihood _, (_:Double) => Uniform(0,1), 0.5, burnIn=0, dropCount=0)
    pullAllSamples(m)
  }

  def timeMetropolisHastings(reps: Int) = run(reps) {
    val m = ArbitraryMetropolisHastings(likelihood _, (_:Double) =>  Uniform(0,1), 0.5, burnIn=burnIn, dropCount=dropCount)
    pullAllSamples(m)
  }

  def timeMetropolisHastingsWithWork(reps: Int) = run(reps) {
    val m = ArbitraryMetropolisHastings(likelihood _, (_:Double) =>  Uniform(0,1), 0.5, burnIn=0, dropCount=dropCount)
    pullAllSamplesWithWork(m)
  }

  def timeThreadedBufferedWithWork(reps: Int) = run(reps) {
    val wrapped = ArbitraryMetropolisHastings(likelihood _, (_:Double) =>  Uniform(0,1), 0.5, burnIn=0, dropCount=dropCount)
    val m = ThreadedBufferedRand(wrapped, bufferSize=bufferSize)
    val result = pullAllSamplesWithWork(m)
    m.stop()
    result
  }
}
