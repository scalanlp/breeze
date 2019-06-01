package breeze.stats.mcmc

/*
 Copyright 2014 Chris Stucchio

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import breeze.stats.distributions._
import breeze.math._
import spire.implicits.cfor
import scala.reflect.ClassTag

trait MetropolisHastings[T] extends Rand[T] {
  def logLikelihood(x: T): Double
  def logTransitionProbability(start: T, end: T): Double

  def proposalDraw(x: T): T // This is a random function, which returns a random y given a deterministic x

  def likelihood(x: T): Double = math.exp(logLikelihood(x))
  def likelihoodRatio(start: T, end: T): Double = math.exp(logLikelihoodRatio(start, end))
  def logLikelihoodRatio(start: T, end: T): Double =
    (logLikelihood(end) - logLikelihood(start) - logTransitionProbability(start, end) + logTransitionProbability(
      end,
      start))
  def rand: RandBasis

  protected def nextDouble: Double = this.rand.generator.nextDouble //uniform random variable
}

trait SymmetricMetropolisHastings[T] extends MetropolisHastings[T] {
  def logTransitionProbability(start: T, end: T): Double = 0.0
  override def logLikelihoodRatio(start: T, end: T): Double = (logLikelihood(end) - logLikelihood(start))
}

trait TracksStatistics { self: MetropolisHastings[_] =>
  /* This obviously can't track stats for you, so you need to build it in.
   * Tracking stats appears to have minimal performance implication, probably
   * because incrementing a few longs is far cheaper than generating random
   * numbers.
   */
  def total: Long
  def acceptanceCount: Long
  def aboveOneCount: Long
  def rejectionCount: Long = total - acceptanceCount
  def aboveOneFrac: Double = aboveOneCount.toDouble / total.toDouble
  def rejectionFrac: Double = rejectionCount.toDouble / total.toDouble
}

abstract class BaseMetropolisHastings[T](logLikelihoodFunc: T => Double, init: T, burnIn: Long = 0, dropCount: Int = 0)(
    implicit val rand: RandBasis = Rand)
    extends MetropolisHastings[T]
    with Process[T]
    with TracksStatistics {
  //Everything but the proposalDraw is implemented

  private var last: T = init
  private var acceptances: Long = 0
  private var totalCount: Long = 0
  private var acceptanceAboveOne: Long = 0

  def logLikelihood(x: T) = logLikelihoodFunc(x)

  def aboveOneCount = acceptanceAboveOne
  def total = totalCount
  def acceptanceCount = acceptances

  private def getNext(): T = {
    totalCount += 1
    val maybeNext = proposalDraw(last)
    val logAcceptanceRatio = logLikelihoodRatio(last, maybeNext)
    if (logAcceptanceRatio > 0.0) { //This is logically unnecessary, but allows us to skip a call to nextDouble
      last = maybeNext
      acceptanceAboveOne += 1
      maybeNext
    } else {
      if (math.log(nextDouble) < logAcceptanceRatio) {
        last = maybeNext
        acceptances += 1
        maybeNext
      } else {
        last
      }
    }
  }

  // Burn in
  cfor(0)(i => i < burnIn, i => i + 1)(i => {
    getNext()
  })
  // end burn in

  def draw(): T = {
    if (dropCount == 0) {
      getNext()
    } else {
      cfor(0)(i => i < dropCount, i => i + 1)(i => {
        getNext()
      })
      getNext()
    }
  }
}

case class ArbitraryMetropolisHastings[T](
    logLikelihood: T => Double,
    val proposal: T => Rand[T],
    val logProposalDensity: (T, T) => Double,
    init: T,
    burnIn: Long = 0,
    dropCount: Int = 0)(implicit rand: RandBasis = Rand)
    extends BaseMetropolisHastings[T](logLikelihood, init, burnIn, dropCount)(rand) {
  def proposalDraw(x: T) = proposal(x).draw()
  def logTransitionProbability(start: T, end: T): Double = logProposalDensity(start, end)

  def observe(x: T) = this.copy(burnIn = 0, init = x)
}

case class AffineStepMetropolisHastings[T](
    logLikelihood: T => Double,
    val proposalStep: Rand[T],
    init: T,
    burnIn: Long = 0,
    dropCount: Int = 0)(implicit rand: RandBasis = Rand, vectorSpace: VectorSpace[T, _])
    extends BaseMetropolisHastings[T](logLikelihood, init, burnIn, dropCount)(rand)
    with SymmetricMetropolisHastings[T] {
  /*
   *  Handles typical case of x => x + random().
   *
   *  Performance motivation: if you use ArbitraryMetropolisHastings,
   *  you are obligated to create an unnecessary Rand[T] object at every step.
   *
   */
  def proposalDraw(x: T): T = vectorSpace.addVV(proposalStep.draw(), x)

  def observe(x: T) = this.copy(burnIn = 0, init = x)
}

case class ThreadedBufferedRand[T](wrapped: Rand[T], bufferSize: Int = 1024 * 8)(implicit m: ClassTag[T])
    extends Rand[T] {
  require(bufferSize > 0)

  private val usedArrayQueue = new java.util.concurrent.LinkedBlockingQueue[Array[T]](2)
  private val newArrayQueue = new java.util.concurrent.LinkedBlockingQueue[Array[T]](2)
  usedArrayQueue.put(new Array[T](bufferSize))
  usedArrayQueue.put(new Array[T](bufferSize))

  @volatile private var stopWorker = false

  private val worker = new Thread {
    override def run(): Unit = {
      while (!stopWorker) {
        val buff = usedArrayQueue.poll(1, java.util.concurrent.TimeUnit.SECONDS)
        if (buff != null) {
          cfor(0)(i => i < bufferSize, i => i + 1)(i => {
            buff(i) = wrapped.draw()
          })
          newArrayQueue.put(buff)
        }
      }
    }
  }
  worker.setDaemon(true)
  worker.setName("worker thread for " + this)
  worker.start()

  private var buffer: Array[T] = newArrayQueue.take()
  private var position: Int = 0

  def stop() = { //In order to allow this class to be garbage collected, you must set this to true.
    stopWorker = true
  }

  def draw(): T = {
    if (position < bufferSize) {
      position += 1
      buffer(position - 1)
    } else {
      usedArrayQueue.put(buffer)
      buffer = newArrayQueue.take()
      position = 1
      buffer(0)
    }
  }
}
