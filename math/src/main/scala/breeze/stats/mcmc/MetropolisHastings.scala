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
import spire.implicits.cfor
import scala.reflect.ClassTag

trait MetropolisHastings[T] extends Rand[T] {
  def logLikelihood: T => Double
  def proposalDraw(x: T): T // This is a random function, which returns a random y given a deterministic x

  def likelihood(x:T): Double = math.exp(logLikelihood(x))
  def likelihoodRatio(x: T, y: T): Double = math.exp(logLikelihood(x) - logLikelihood(y))

  protected val uniformDist: Uniform

  protected def checkUniformDist(uniform: Uniform): Uniform = {
    require(uniform.low == 0.0)
    require(uniform.high == 1.0)
    uniform
  }
}

trait TracksAcceptancesRejections { self:MetropolisHastings[_] =>
  def acceptances: Long
  def rejections: Long
  def acceptanceRatio: Double = acceptances.toDouble / (acceptances + rejections).toDouble
}

abstract class BaseMetropolisHastings[T](val logLikelihood: T => Double, init: T, uniform: Option[Uniform] = None, burnIn: Long = 0, dropCount: Int = 0)(implicit rand:RandBasis=Rand) extends MetropolisHastings[T] with Process[T] {
  //Everything but the proposalDraw is implemented

  override protected val uniformDist: Uniform = checkUniformDist(uniform.getOrElse({ Uniform(0,1) }))

  private var last: T = init

  private def getNext(): T = {
    val maybeNext = proposalDraw(last)
    val acceptanceRatio = likelihoodRatio(maybeNext, last)
    if (acceptanceRatio > 1.0) { //This is logically unnecessary, but allows us to skip a call to uniformDist.draw()
      last = maybeNext
      maybeNext
    } else {
      if (uniformDist.draw() < acceptanceRatio) {
        last = maybeNext
        maybeNext
      } else {
        last
      }
    }
  }

  // Burn in
  cfor(0)(i => i< burnIn, i => i+1)(i => {
    getNext()
  })
  // end burn in

  def draw(): T = {
    if (dropCount == 0) {
      getNext()
    } else {
      cfor(0)(i => i < dropCount, i => i+1)(i => {
        getNext()
      })
      getNext()
    }
  }
}

case class ArbitraryMetropolisHastings[T](override val logLikelihood: T => Double, val proposal: T => Rand[T], init: T, uniform: Option[Uniform] = None, burnIn: Long = 0, dropCount: Int = 0)(implicit rand:RandBasis=Rand) extends BaseMetropolisHastings[T](logLikelihood, init, uniform, burnIn, dropCount)(rand) {
  def proposalDraw(x: T) = proposal(x).draw()

  def observe(x: T) = this.copy(burnIn=0, init = x)
}

abstract class BaseThreadedBufferedMetropolisHastings[T](val logLikelihood: T => Double, init: T, uniform: Option[Uniform] = None, burnIn: Long = 0, dropCount: Int = 0, bufferSize: Int = 1024*8)(implicit m: ClassTag[T]) extends MetropolisHastings[T] with ThreadedMCMC {
  require(bufferSize > 0)

  override protected val uniformDist: Uniform = checkUniformDist(uniform.getOrElse({ Uniform(0,1) }))

  private val queue = new java.util.concurrent.LinkedBlockingQueue[Array[T]](2)
  queue.put(new Array[T](bufferSize))
  queue.put(new Array[T](bufferSize))

  @volatile private var stopWorker = false

  private val worker = new Thread {
    override def run() {
      var last = init

      //Burn in
      cfor(0)(i => i< burnIn, i => i+1)(i => {
        last = getNext(last)
      })

      while (!stopWorker) {
        val buff = queue.poll(1, java.util.concurrent.TimeUnit.SECONDS)
        if (buff != null) {
          cfor(0)(i => i<bufferSize, i => i+1)(i => {
            cfor(0)(j => j<dropCount, j => j+1)(j => {
              last = getNext(last)
            })
            last = getNext(last)
            buff(i) = last
          })
          queue.put(buff)
        }
      }
    }
  }
  worker.setDaemon(true)
  worker.setName("worker thread for " + this)
  worker.start()

  private var buffer: Array[T] = queue.take()
  private var position: Int = 0

  private def getNext(last: T): T = {
    val maybeNext = proposalDraw(last)
    val acceptanceRatio = likelihoodRatio(maybeNext, last)
    if (acceptanceRatio > 1.0) {
      maybeNext
    } else {
      if (uniformDist.draw() < acceptanceRatio) {
        maybeNext
      } else {
        last
      }
    }
  }

  def stop() = { //In order to allow this class to be garbage collected, you must set this to true.
    stopWorker = true
  }

  def draw(): T = {
    if (position < bufferSize) {
      position += 1
      buffer(position-1)
    } else {
      queue.put(buffer)
      buffer = queue.take()
      position = 1
      buffer(0)
    }
  }
}

object ArbitraryThreadedBufferedMetropolisHastings {
  def apply[T](logLikelihood: T => Double, proposal: T => Rand[T], init: T, uniform: Option[Uniform] = None, burnIn: Long = 0, dropCount: Int = 0, bufferSize: Int = 1024*8)(implicit m: ClassTag[T]) = new ArbitraryThreadedBufferedMetropolisHastings[T](logLikelihood, proposal, init, uniform, burnIn, dropCount, bufferSize)(m)
}

class ArbitraryThreadedBufferedMetropolisHastings[T](override val logLikelihood: T => Double, val proposal: T => Rand[T], init: T, uniform: Option[Uniform] = None, burnIn: Long = 0, dropCount: Int = 0, bufferSize: Int = 1024*8)(m: ClassTag[T]) extends BaseThreadedBufferedMetropolisHastings(logLikelihood, init, uniform, burnIn, dropCount, bufferSize)(m) {
  def proposalDraw(x: T) = proposal(x).draw()
}
