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

import java.util.Arrays;
import counters._;
import util.Index;
import math.Numerics._;
import stats.sampling._
import Math._;
import util.Implicits._;

/**
* Represents a single-markov CRF, can score sequences and such.
*
* @param S: hidden state type
* @param T: observed element type
* @param features, functions of the form (previousState,nextState,i,obs)=&lt;Double
* @param weights, to go with the features
* @param stateDict: a set of allowed states
* @param startState: an initial state, that seq's implicitly start with. Must be in stateDict
* @param window: how wide of a window the features are over.
*                You're to be on your best behavior not to violate this.
*/
final class CRF[S,T](val features: Seq[(Seq[S],Int,Seq[T])=>Double],
               val weights: Seq[Double],
               val stateDict: Set[S],
               val startState: S,
               val window: Int) {
  require(features.length == weights.length);
  require(window > 0)

  private val stateIndex = new Index[S];
  private val start = stateIndex.apply(startState);
  stateIndex.indexAll(stateDict);

  def calibrate(words: Seq[T]) = new Calibration(words);

  class Calibration(val words:Seq[T]) {
    private lazy val fScores = forwardScores(words);
    private lazy val bScores = backwardScores(words);

    lazy val partition = exp(logPartition);
    lazy val logPartition = {
      logSum(fScores.last);
    }

    lazy val logMarginals = {
      val z = logPartition;

      val preResult = new Array[Array[Double]](words.length, stateIndex.size);
      for(i <- 0 until words.length; 
          stateSeq <- 0 until fScores(i).size) { // each possible assignment to the left and right message
          val states = decode(stateSeq,window); 
          val fbScore = fScores(i)(stateSeq) + bScores(i)(stateSeq); 
          // sum out this contribution
          preResult(i)(states.last) = logSum(preResult(i)(states.last),fbScore);
      }

      val result = for(arr <- preResult) yield {
        val c = DoubleCounter[S]();
        // subtract out the log partition function.
        c ++= arr.zipWithIndex filter { case (v,_) => !v.isInfinite } map { case (v,i) => ((stateIndex unapply i),v - z)}
        c;
      }

      result;
    }
  }

  private def forwardScores(words: Seq[T]) = {
    var prevLogProbs = new Array[Double](pow(stateIndex.size,window).toInt);
    Arrays.fill(prevLogProbs,NEG_INF_DOUBLE);
    prevLogProbs( encode( (1 to window).map( _ => start))) = 0.0;

    val result = new Array[Array[Double]](words.length,prevLogProbs.size);

    for {
      i <- 0 until words.length
    } /* do */ {
      val nextLogProbs = result(i);
      Arrays.fill(nextLogProbs,NEG_INF_DOUBLE);
      for { 
        // for each prior sequence of states  x_i,... x_{i+window}
        stateSeq <- 0 until prevLogProbs.length;
        states = decode(stateSeq,window);
        // and for each next state x_{i+window+1}
        nextState <- 0 until stateIndex.size
        val nextStates = states.tail ++ List(nextState);
        val nextStateSeq = encode(nextStates)
      } /* do */ { // sum out the x_i
        var score = prevLogProbs(stateSeq);
        if(score != NEG_INF_DOUBLE) {
          for( (f,w) <- features.elements zip weights.elements) {
            score += w * f(nextStates map (stateIndex unapply _),i,words);
          }
          nextLogProbs(nextStateSeq) = logSum(nextLogProbs(nextStateSeq),score);
        }
      }
      prevLogProbs = nextLogProbs;
    }

    result;
  }

  private def backwardScores(words: Seq[T]) = {
    val result = new Array[Array[Double]](words.length,pow(stateIndex.size,window).toInt);
    var prevLogProbs = result.last;
    Arrays.fill(prevLogProbs,0.0);
    prevLogProbs( encode( (1 to window).map( _ => start))) = 0.0;
   
    for {
      i <- (words.length-2) to 0 by -1
    } /* do */ {
      val nextLogProbs = result(i);
      Arrays.fill(nextLogProbs,NEG_INF_DOUBLE);
      for {
        stateSeq <- 0 until prevLogProbs.length;
        states = decode(stateSeq,window);
        nextState <- 0 until stateIndex.size
        nextStates = nextState:: states.take(window-1);
        nextStateSeq = encode(nextStates)
      } /* do */ {
        var score = prevLogProbs(stateSeq);
        if(score != NEG_INF_DOUBLE) {
          for( (f,w) <- features.elements zip weights.elements) {
            score += w * f(nextStates map (stateIndex unapply _),i,words);
          }
          nextLogProbs(nextStateSeq) = logSum(nextLogProbs(nextStateSeq),score);
        }
      }
      prevLogProbs = nextLogProbs;
    }

    result;
  }

  private def encode(s: Seq[Int]) = {
    s.foldRight(0)(_ + _ * stateIndex.size);
  }

  private def decode(s: Int, dim: Int) = {
    val result = s unfoldr  { s =>
      if(s < 0) {
        None
      } else if(s < stateIndex.size) {
        Some( (s,-1) )
      } else {
        Some( (s%stateIndex.size,s/stateIndex.size) )
      }
    };

    (result ++ (1 to dim).map(_ => 0)).take(dim)
  }

}
