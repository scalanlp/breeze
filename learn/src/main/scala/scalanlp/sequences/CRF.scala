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

import scalanlp.util.Lazy.Implicits._;
import scala.math.{pow,exp,log}
import scalala.tensor.dense.DenseVector.zeros
import scalala.library.Numerics._;
import scalala.library.Library.{mean,norm,softmax}
import sparse.SparseVector
import tensor.sparse.OldSparseVector
import util.{Index, Encoder, Lazy}
import scala.collection.immutable.BitSet
import scalala.library.Numerics
import java.util.Arrays

trait CRFModel[L,W] extends Encoder[L] {
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
      require(pos > 0 && pos <= length, "pos must be <= length, but got " + pos)
      val left = if(pos == 0) {
        initialMessage
      } else {
        forward(pos)
      }

      val right = if(pos == length) {
        initialMessage
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


