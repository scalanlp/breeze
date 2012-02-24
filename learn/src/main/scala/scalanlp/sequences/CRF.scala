package scalanlp.sequences
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

import scala.collection.mutable.ArrayBuffer;

import scalala.tensor.mutable.Vector;
import scalala.tensor.dense._;
import scalala.tensor._;

import scalanlp._;

import data.Example
import optimize.FirstOrderMinimizer.OptParams
import optimize.{CachedBatchDiffFunction, RandomizedGradientCheckingFunction, BatchDiffFunction, DiffFunction}
import scalanlp.util.Lazy.Implicits._;
import scala.math.{pow,exp,log}
import scalala.tensor.dense.DenseVector.zeros
import scalala.library.Numerics._;
import scalala.library.Library.{mean,norm,softmax}
import sparse.SparseVector
import tensor.sparse.OldSparseVector
import util.{Index, Encoder, Lazy}
import scalala.library.Numerics
import java.util.Arrays
import scala.collection.immutable.{Range, BitSet}

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
 * Represents a linear-chain CRF with a fixed window size (2), can score sequences and such.
 *
 * W is kept as an opaque object, which allows for you to condition on more than just the "outputs"
 *
 */
class CRF[L,W](val transitions: CRFModel[L,W]) {
  def numStates = transitions.index.size

  def viterbi(words: W, length: Int) = {
    val forwardScores = Array.fill(length)(mkVector(numStates, Double.NegativeInfinity))
    val back = Array.fill(length,numStates)(-1)

    var previous : Vector[Double] = initialMessage;

    // forward
    for(i <- 0 until length) {
      val cur = forwardScores(i)
      // TODO: add in active iterators to scalala?
      for ( next <- transitions.validSymbols(i,words)) {
        for((previousLabel,prevScore) <- previous.pairsIterator) {
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

    var previous : Vector[Double] = initialMessage;

    val cache = transitions.fillArray(0.0)
    // forward
    for(i <- 0 until length) {
      val cur = forwardScores(i)
      // TODO: add in active iterators to scalala?
      for ( next <- transitions.validSymbols(i,words)) {
        var offset = 0
        for((previousLabel,prevScore) <- previous.pairsIterator) {
          val score = transitions.score(i,words,previousLabel,next) + prevScore
          if(score != Double.NegativeInfinity) {
            cache(offset) = score
            offset += 1
          }
        }
        cur(next) = Numerics.logSum(cache,offset)
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
        for((next,prevScore) <- previous.pairsIterator) {
          val score = transitions.score(i+1,words,previousLabel,next) + prevScore
          if(score != Double.NegativeInfinity) {
            cache(offset) = score
            offset += 1
          }
        }
        cur(previousLabel) = Numerics.logSum(cache,offset)

      }
      previous = cur
    }


    new Calibration(words, length, forwardScores, backwardScores)
  }


  protected def initialMessage:Vector[Double] = {
    val r = new OldSparseVector(numStates,Double.NegativeInfinity)
    r(transitions.start) = 0.0
    r
  }

  protected def mkVector(size:Int, fill: Double):Vector[Double] = {
    val data = new Array[Double](size);
    java.util.Arrays.fill(data,fill);
    val v = new DenseVectorCol(data);
    v;
  }

  /**
  * A calibration is a class that represents the CRF being conditioned on some input.
  */
  class Calibration protected[CRF] (val words:W, length: Int, forward: Array[Vector[Double]], backward: Array[Vector[Double]]) {
    /** 
    * returns the value of the partition function for this sequence of words. This is the normalizer for the distribution.
    */
    def partition = exp(logPartition);


    /**
    * returns the value of the log partition function for this sequence of words.
    */
    lazy val logPartition = {
      val result = softmax(forward.last + backward.last)
      assert(!result.isNaN);
      result;
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
      for( (l,ls) <- left.pairsIterator if ls != Double.NegativeInfinity;
          (r,rs) <- right.pairsIterator if rs != Double.NegativeInfinity) {
        result(l,r) = ls + rs + transitions.score(pos,words,l,r) -logPartition
      }
      result
    }

  }

}

object CRF {
  trait Feature
  trait Featurizer[L,W] extends Encoder[Feature] with Serializable {
    def featuresFor(pos: Int, w: W, l: Int, ln: Int):Iterator[Int]
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
      val model = mkModel(labels,weights.data)
      new CRF(model)
    }

    def mkModel(labels: Index[L], weights: Array[Double]): CRFModel[L, W] = {
      new CRFModel[L, W] {
        val index = labels

        def score(pos: Int, w: W, l: Int, ln: Int) = {
          var score = 0.0
          for (f <- featurizer.featuresFor(pos, w, l, ln)) {
            score += weights(f)
          }
          score
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
          val weights = x.data

          val model = mkModel(labels, weights)

          val crf = new CRF(model)

          val b = batch.map(dataset)
//          println(b)
//          println(b.size)
          val r  = b.par.aggregate(null:(Double,DenseVector[Double])) ({ (pair,ex) =>
            val (score,counts) = if(pair eq null) (0.0,featurizer.mkDenseVector()) else pair
            val (goldScore,goldFeatures) = computeGoldFeatures(ex, weights, counts.data, -1)
            val (part,guessFeatures) = computeGuessFeatures(ex, crf, counts.data, 1)
            (score + part - goldScore,counts)
          }, {(a,b) =>
            (a._1 + b._1, a._2 += b._2)
          });

          println(r._1)
          r
        }

        private def computeGoldFeatures(ex: Example[Seq[L], (W,Int)], weights: Array[Double], counts: Array[Double], scale: Double) = {
          val gold = (startSymbol +: ex.label).map(labels)
          var score = 0.0
          for(i <- 0 until ex.features._2; f <- featurizer.featuresFor(i,ex.features._1,gold(i),gold(i+1))) {
            counts(f) += 1 * scale
            score += weights(f)
          }
          (score -> counts)

        }

        private def computeGuessFeatures(ex: Example[Seq[L], (W,Int)], crf: CRF[L,W], counts: Array[Double], scale: Double) = {
          val cal = crf.calibrate(ex.features._1, ex.features._2)
          val score = cal.logPartition
          val edgeMarginals = Array.tabulate(ex.features._2)(i => cal.edgeMarginalAt(i))
          def validSymbols(i: Int) = if(i < 0) BitSet(crf.transitions.start) else crf.transitions.validSymbols(i,ex.features._1)

          for(i <- 0 until ex.features._2;
              prev <- validSymbols(i-1);
              next <- validSymbols(i);
              m = scala.math.exp(edgeMarginals(i)(prev,next));
              f <- featurizer.featuresFor(i,ex.features._1, prev, next)) {
            counts(f) += m * scale
          }

          (score -> counts)
        }

        def fullRange = (0 until dataset.length)
      }

    }
  }
}