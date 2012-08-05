package breeze.sequences
/*
 Copyright 2009 David Hall, Daniel Ramage

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

import scala.collection.mutable.ArrayBuffer

import breeze._
import linalg._
import numerics._

import data.Example
import optimize.FirstOrderMinimizer.OptParams
import optimize.{CachedBatchDiffFunction, RandomizedGradientCheckingFunction, BatchDiffFunction, DiffFunction}
import util.{Index, Encoder, Lazy}
import java.util.Arrays
import scala.collection.immutable.{Range, BitSet}


/**
 * Trait to handle the scoring for each transiton in a CRF
 * @tparam L label type
 * @tparam W the sequence type. Note that we don't take Seq[W], but a single W.
 * @author dlwh
 */
trait CRFModel[L,W] extends Encoder[L] with Serializable {
  val index: Index[L]
  def scoreTransition(pos: Int, w: W, l: L, ln: L):Double = {
    score(pos,w,index(l),index(ln))
  }
  def score(pos: Int, w: W, l: Int, ln: Int):Double
  def validSymbols(pos: Int, w: W):BitSet
  val startSymbol: L
  val start: Int
}

/**
 * Represents a linear-chain Conditional Random Field with a fixed window size (2), can score sequences and such.
 *
 * W is kept as an opaque object, which allows for you to condition on more than just the "outputs"
 * @tparam L label type
 * @tparam W the sequence type. Note that we don't take Seq[W], but a single W.
 * @param transitions The underlying CRFModel, which scores transitions
 * @author dlwh
 */
class CRF[L,W](val transitions: CRFModel[L,W]) {
  def numStates = transitions.index.size

  /**
   * Computes the most likely series of states for the
   * @param words input sequence
   * @param length the length of the sequence
   * @return most likely sequence
   */
  def viterbi(words: W, length: Int) = {
    val forwardScores = Array.fill(length)(mkVector(numStates, Double.NegativeInfinity))
    val back = Array.fill(length,numStates)(-1)

    var previous : Vector[Double] = initialMessage

    // forward
    for(i <- 0 until length) {
      val cur = forwardScores(i)
      for ( next <- transitions.validSymbols(i,words)) {
        for((previousLabel,prevScore) <- previous.iterator) {
          val score = transitions.score(i,words,previousLabel,next) + prevScore
            if(score > cur(next)) {
              cur(next) = score
              back(i)(next) = previousLabel
            }
        }
      }

      previous = cur
    }

    val derivation = ArrayBuffer[L]()
    var prev = forwardScores(length-1).argmax
    derivation += transitions.index.get(prev)
    for(i <- (length-2) to 0 by -1) {
      prev = back(i+1)(prev)
      derivation += transitions.index.get(prev)
    }

    derivation.reverse : IndexedSeq[L]
  }

  /**
  * Conditions the CRF on an input and a length, which gives you access
  * to the marginals for each hidden state.
  */
  def calibrate(words: W, length: Int) = {
    val forwardScores = Array.fill(length)(mkVector(numStates, Double.NegativeInfinity))

    var previous : Vector[Double] = initialMessage

    val cache = transitions.fillArray(0.0)
    // forward
    for(i <- 0 until length) {
      val cur = forwardScores(i)
      // TODO: add in active iterators to scalala?
      for ( next <- transitions.validSymbols(i,words)) {
        var offset = 0
        for((previousLabel,prevScore) <- previous.iterator) {
          val score = transitions.score(i,words,previousLabel,next) + prevScore
          if(score != Double.NegativeInfinity) {
            cache(offset) = score
            offset += 1
          }
        }
        cur(next) = numerics.logSum(cache,offset)
      }


      previous = cur
    }

    //backward
    val backwardScores = Array.fill(length)(mkVector(numStates, Double.NegativeInfinity))
    backwardScores(length-1) =  DenseVector.fill(numStates)(0.0)
    previous = backwardScores.last

    for(i <- (length-2) to 0 by -1) {
      val cur = backwardScores(i)
      // TODO: add in active iterators to scalala?
      for ( previousLabel <- transitions.validSymbols(i,words)) {
        var offset = 0
        for((next,prevScore) <- previous.iterator) {
          val score = transitions.score(i+1,words,previousLabel,next) + prevScore
          if(score != Double.NegativeInfinity) {
            cache(offset) = score
            offset += 1
          }
        }
        cur(previousLabel) = numerics.logSum(cache,offset)

      }
      previous = cur
    }

    new Calibration(words, length, forwardScores, backwardScores)
  }


  protected def initialMessage:Vector[Double] = {
    val r = mkVector(numStates, Double.NegativeInfinity)
//    val r = new OldSparseVector(numStates,Double.NegativeInfinity)
    r(transitions.start) = 0.0
    r
  }

  protected def mkVector(size:Int, fill: Double):Vector[Double] = {
    val data = new Array[Double](size)
    java.util.Arrays.fill(data,fill)
    val v = new DenseVector(data)
    v
  }

  /**
   * A calibration is a class that represents the CRF being conditioned on some input.
   *
   * You can get marginals and expected counts from it.
   */
  class Calibration protected[CRF] (val words:W, length: Int, forward: Array[Vector[Double]], backward: Array[Vector[Double]]) {
    /** 
    * returns the value of the partition function for this sequence of words. This is the normalizer for the distribution.
    */
    def partition = exp(logPartition)


    /**
    * returns the value of the log partition function for this sequence of words.
    */
    lazy val logPartition = {
      val result = softmax(forward.last + backward.last)
      assert(!result.isNaN)
      result
    }

    def marginalAt(pos: Int) = {
      require(pos >= 0 && pos < length, "pos must be in length, but got " + pos)
      (forward(pos) + backward(pos)) - logPartition
    }

    /**
     * Returns the edge marginal for (pos-1,pos). pos <= length
     */
    def edgeMarginalAt(pos :Int) = {
      require(pos >= 0 && pos < length, "pos must be <= length, but got " + pos)
      val left = if(pos == 0) {
        initialMessage
      } else {
        forward(pos-1)
      }

      val right = if( pos == length) {
        transitions.mkDenseVector()
      } else {
        backward(pos)
      }

      val result = DenseMatrix.zeros[Double](numStates,numStates)
      Arrays.fill(result.data, Double.NegativeInfinity)
      for( (l,ls) <- left.iterator if ls != Double.NegativeInfinity;
          (r,rs) <- right.iterator if rs != Double.NegativeInfinity) {
        result(l,r) = ls + rs + transitions.score(pos,words,l,r) -logPartition
      }
      result
    }

  }

}

object CRF {
  trait Feature
  trait Featurizer[L,W] extends Encoder[Feature] with Serializable {
    def featuresFor(pos: Int, w: W, l: Int, ln: Int):SparseVector[Double]
  }

  class Trainer[L,W](featurizer: Featurizer[L,W],
                     startSymbol: L,
                     params: OptParams) { outer =>

    def train(dataset: IndexedSeq[Example[Seq[L],(W,Int)]]) = {
      val ss = startSymbol
      val labels = Index(Iterator(ss) ++ dataset.iterator.flatMap(_.label))

      val obj: BatchDiffFunction[DenseVector[Double]] = objective(dataset, labels)
      val cached = new CachedBatchDiffFunction(obj)
//      val checking = new RandomizedGradientCheckingFunction(cached)
      val weights = params.iterations(cached,featurizer.mkDenseVector()).drop(params.maxIterations).next.x
      val model = mkModel(labels,weights)
      new CRF(model)
    }

    def mkModel(labels: Index[L], weights: DenseVector[Double]): CRFModel[L, W] = {
      new CRFModel[L, W] {
        val index = labels

        def score(pos: Int, w: W, l: Int, ln: Int) = {
          weights dot featurizer.featuresFor(pos, w, l, ln)
        }

        def validSymbols(pos: Int, w: W) = {
          BitSet.empty ++ (0 until labels.size)
        }

        val startSymbol = outer.startSymbol
        val start = index(outer.startSymbol)
      }
    }

    def objective(dataset: IndexedSeq[Example[Seq[L],(W,Int)]],
                  labels: Index[L]): BatchDiffFunction[DenseVector[Double]]  = {
      new BatchDiffFunction[DenseVector[Double]] {
        def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]) = {
          val model = mkModel(labels, x)

          val crf = new CRF(model)

          val b = batch.map(dataset)
//          println(b)
//          println(b.size)
          val r  = b.par.aggregate(null:(Double,DenseVector[Double])) ({ (pair,ex) =>
            val (score,counts) = if(pair eq null) (0.0,featurizer.mkDenseVector()) else pair
            val (goldScore,goldFeatures) = computeGoldFeatures(ex, x, counts.data, -1)
            val (part,guessFeatures) = computeGuessFeatures(ex, crf, counts.data, 1)
            (score + part - goldScore,counts)
          }, {(a,b) =>
            (a._1 + b._1, a._2 += b._2)
          })

          println(r._1)
          r
        }

        private def computeGoldFeatures(ex: Example[Seq[L], (W,Int)], weights: Vector[Double], counts: Array[Double], scale: Double) = {
          val gold = (startSymbol +: ex.label).map(labels)
          var score = 0.0
          for(i <- 0 until ex.features._2; (f,v) <- featurizer.featuresFor(i,ex.features._1,gold(i),gold(i+1)).activeIterator) {
            counts(f) += 1 * scale * v
            score += weights(f)
          }
          (score -> counts)

        }

        private def computeGuessFeatures(ex: Example[Seq[L], (W,Int)], crf: CRF[L,W], counts: Array[Double], scale: Double) = {
          val cal = crf.calibrate(ex.features._1, ex.features._2)
          val score = cal.logPartition
          val edgeMarginals = Array.tabulate(ex.features._2)(i => cal.edgeMarginalAt(i))
          def validSymbols(i: Int) = if(i < 0) BitSet(crf.transitions.start) else crf.transitions.validSymbols(i,ex.features._1)

          for(i <- 0 until ex.features._2; prev <- validSymbols(i-1); next <- validSymbols(i)) {
            val m = scala.math.exp(edgeMarginals(i)(prev,next))
            var off = 0
            val features = featurizer.featuresFor(i,ex.features._1, prev, next)
            while(off < features.activeSize) {
              val f = features.indexAt(off)
              val v = features.valueAt(off)
              counts(f) += m * scale * v
              off += 1
            }
          }

          (score -> counts)
        }

        def fullRange = (0 until dataset.length)
      }

    }
  }
}