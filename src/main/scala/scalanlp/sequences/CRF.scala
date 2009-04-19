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
import counters.ints._;
import util.Index;
import math.Numerics._;
import stats.sampling._
import Math._;
import util.Implicits._;
import scala.collection.mutable.ArrayBuffer;
import scalala.tensor.sparse._;

/**
* Represents a single-markov CRF, can score sequences and such.
*
* @param features, functions of the form (previousState,nextState,i,observations)=&lt;Double
* @param weights, to go with the features
* @param stateDict: a set of allowed states
* @param start: an initial state, that seq's implicitly start with. Must be in stateDict
* @param window: how wide of a window the features are over.
*                You're to be on your best behavior not to violate this.
*/
final class CRF(val features: Seq[(Seq[Int],Int,Seq[Int])=>Double],
               val weights: Seq[Double],
               val numStates: Int,
               val start: Int,
               val validStatesForObservation: Map[Int,Collection[Int]],
               val window: Int) {
  require(features.length == weights.length);
  require(window > 0)

  def calibrate(words: Seq[Int]) = new Calibration(words);

  class Calibration(val words:Seq[Int]) {
    private lazy val fScores : Seq[Message] = {
      val firstMessage = new Message(-1,0);
      firstMessage.scores(encode(Array.make(window-1,start))) = 0.0;
      val messages = new ArrayBuffer[Message];
      messages += firstMessage;
      for(f <- factors) {
        messages += f.leftMessage(messages.last);
      }
      messages.take(words.length);
    }

    private lazy val bScores: Seq[Message] = {
      val firstMessage = new Message(-1,words.length-1);
      // first message is: p(x|w) \propto 1 forall valid state sequences x
      for(seq <- 0 until firstMessage.scores.length;
          states = decode(seq,window-1)) {
        val allowed = states.elements.zipWithIndex forall { case(state,i) => 
          validStatesFor(words.length - window + i) contains state
        }
        if(allowed) firstMessage.scores(seq) = 0.0;
        else firstMessage.scores(seq) = NEG_INF_DOUBLE;
      }
      val messages = new ArrayBuffer[Message];
      messages += firstMessage;
      for(f <- factors.reverse) {
        messages += f.rightMessage(messages.last);
      }
      messages.reverse.drop(1)
    }

    private val factors = (0 until words.length).toArray.map( (i:Int) => new Factor(i));

    class Factor(pos: Int) {
      assert(pos <= words.length);
      assert(pos >= 0,pos + "");

      val cache = new Array[Double](pow(numStates,window).toInt);
      Arrays.fill(cache,Double.NaN);

      def compute(stateSeq: Int) = {
        if(!cache(stateSeq).isNaN) {
          cache(stateSeq)
        } else {
          var i = 0;
          var score = 0.0;
          val states = decode(stateSeq);
          while(i < features.length && !score.isInfinite) {
            score += weights(i) * features(i)(states,pos,words);
            i += 1;
          }
          assert(!score.isNaN);
          cache(stateSeq) = score;
          score;
        }
      }

      def  leftMessage(incoming: Message) = {
        val outgoing = new Message(pos,pos+1);
        for { 
          // for each prior sequence of states  x_i,... x_{i+window}
          stateSeq <- 0 until incoming.scores.length;
          val initScore = incoming.scores(stateSeq);
          if initScore != NEG_INF_DOUBLE 
          // and for each next state x_{i+window+1}
          nextState <- validStatesFor(pos)
        } /* do */ { // sum out the x_i
          val nextStateSeq = appendRight(stateSeq, nextState);
          val outgoingAssignment = shiftRight(nextStateSeq,0);
          val score = initScore + compute(nextStateSeq);
          outgoing.scores(outgoingAssignment) = logSum(outgoing.scores(outgoingAssignment),score);
          assert(!outgoing.scores(outgoingAssignment).isNaN);
        }
        outgoing
      }

      def rightMessage(incoming: Message) = {
        val outgoing = new Message(pos,pos-1);
        for { 
          // for each future sequence of states  x_{i+1},... x_{i+window}
          stateSeq <- 0 until incoming.scores.length;
          val initScore = incoming.scores(stateSeq);
          if initScore != NEG_INF_DOUBLE 
          // and for each next state x_{i}
          nextState <- validStatesFor(pos-window+1)
        } /* do */ { // sum out the x_{i+window}
          val nextStateSeq = shiftLeft(stateSeq,nextState);
          val score = initScore + compute(nextStateSeq);
          val outgoingAssignment = shiftLeft(nextStateSeq,0) / numStates;
          if(pos == 38) {
            println("i38")
            println(decode(nextStateSeq));
            println(decode(stateSeq,window-1));
            println(decode(outgoingAssignment,window-1));
            println(score);
            println();
          }
          outgoing.scores(outgoingAssignment) = logSum(outgoing.scores(outgoingAssignment),score);
          assert(!outgoing.scores(outgoingAssignment).isNaN);
        }
        outgoing;
      }
    }

    def validStatesFor(pos: Int) = {
      if(pos < 0) Iterator.single(start);
      else validStatesForObservation(words(pos)).elements;
    }

    case class Message(src: Int, dest: Int) {
      val scores = new Array[Double](pow(numStates,window-1).toInt);
      Arrays.fill(scores,NEG_INF_DOUBLE);
    }

    lazy val partition = exp(logPartition);
    lazy val logPartition = {
      logSum(fScores.last.scores);
    }

    def renderMessage(arr: Array[Double]) = {
      arr.zipWithIndex foreach { case(v,seq) =>
        if(!v.isInfinite) {
          val states = decode(seq,window-1);
          println(states + " =>  " + v);
        }
      }
    }

    lazy val calibratedFactors = {
      println(factors.length,bScores.length,fScores.length);
      println(words.length);
      assert(factors.length == bScores.length);
      assert(factors.length == fScores.length);
      for(i <- 0 until factors.length) yield {
        val output = new Array[Double](pow(numStates,window).toInt);
        val left = fScores(i).scores;
        val right = bScores(i).scores;
        println(fScores(i) + " " + bScores(i));
        println("Left {");
        renderMessage(left);
        println("}");
        println("Right {");
        renderMessage(right);
        println("}");
        var seq = 0; //22% of time spent here if it's a forloop.
        while(seq < output.length) {
          val ls = shiftLeft(seq,0) / numStates;
          val rs = shiftRight(seq,0);
          output(seq)  = left(ls) + right(rs);
          if(!output(seq).isInfinite) {
            output(seq) += factors(i).compute(seq);
          }
          seq += 1;
        }
        output;
      }
    }

    lazy val logMarginals = {
      val z = logPartition;

      val preResult = new Array[Array[Double]](words.length, numStates);
      for(arr <- preResult) {
        Arrays.fill(arr,NEG_INF_DOUBLE);
      }
      for(i <- 0 until words.length;
          factor = calibratedFactors(i);
          stateSeq <- 0 until factor.size) { // each possible assignment to the left and right message
          val head = stateSeq % numStates; // trigram XYZ, get X
          // sum out this contribution
          preResult(i)(head) = logSum(preResult(i)(head),factor(stateSeq));
      }

      val result = for(arr <- preResult) yield {
        val c = Int2DoubleCounter();
        // subtract out the log partition function.
        c ++= arr.zipWithIndex filter { case (v,_) => println(v); !v.isInfinite } map {case (v,k) => (k,v - z)}
        assert(c.size > 0)
        c;
      }

      result;
    }
  }

/*
  private def forwardScores(words: Seq[Int]) = {
    var prevLogProbs = new Array[Double](pow(numStates,window).toInt);
    Arrays.fill(prevLogProbs,NEG_INF_DOUBLE);
    prevLogProbs( encode( (1 to window).map( _ => start))) = 0.0;

    val result = new Array[Array[Double]](words.length,prevLogProbs.size);

    for {
      i <- 0 until words.length
    } {
      val nextLogProbs = result(i);
      Arrays.fill(nextLogProbs,NEG_INF_DOUBLE);
      for { 
        // for each prior sequence of states  x_i,... x_{i+window}
        stateSeq <- 0 until prevLogProbs.length;
        val initScore = prevLogProbs(stateSeq);
        if initScore != NEG_INF_DOUBLE 
        // and for each next state x_{i+window+1}
        nextState <- validStatesForObservation(words(i))
      } { // sum out the x_i
        var score = initScore;
        assert(!score.isNaN);
        val nextStateSeq = shiftRight(stateSeq,nextState);
        val nextStates = decode(nextStateSeq);
        for( (f,w) <- features.elements zip weights.elements) {
          score += w * f(nextStates,i,words);
          assert(!score.isNaN);
        }
        assert(!score.isNaN);
        println(score + " " + nextLogProbs(nextStateSeq) + " " + logSum(nextLogProbs(nextStateSeq),score));
        nextLogProbs(nextStateSeq) = logSum(nextLogProbs(nextStateSeq),score);
        assert(!nextLogProbs(nextStateSeq).isNaN);
      }
      prevLogProbs = nextLogProbs;
    }

    result;
  }


  private def backwardScores(words: Seq[Int]) = {
    val result = new Array[Array[Double]](words.length,pow(numStates,window).toInt);
    var prevLogProbs = null;
    for(i <- 1 until window) {
      prevLogProbs = result(result.length-i)
      Arrays.fill(prevLogProbs,0.0);
    }

    for {
      i <- (words.length-window) to 0 by -1
    }  {
      val nextLogProbs = result(i);
      Arrays.fill(nextLogProbs,NEG_INF_DOUBLE);
      for {
        stateSeq <- 0 until prevLogProbs.length;
        val initScore = prevLogProbs(stateSeq);
        if initScore != NEG_INF_DOUBLE 
        nextState <- validStatesForObservation(words(i))
        val nextStateSeq = shiftLeft(stateSeq,nextState);
        val nextStates = decode(nextStateSeq)
      }  {
        var score = initScore;
        assert(!score.isNaN);
        for( (f,w) <- features.elements zip weights.elements) {
          score += w * f(nextStates,i,words);
        }
        nextLogProbs(nextStateSeq) = logSum(nextLogProbs(nextStateSeq),score);
      }
      prevLogProbs = nextLogProbs;
    }

    result;
  }
  */

  val rightShifter = pow(numStates,window-1).toInt;
  def shiftRight(s: Int, next: Int) = {
    s / numStates + rightShifter * next;
  }

  def appendRight(s: Int, next:Int) = {
    assert(s < rightShifter);
    s + rightShifter * next;
  }

  def shiftLeft(s:Int, prev: Int) = {
    s % rightShifter * numStates + prev;
  }

  def encode(s: Seq[Int]) = {
    s.foldRight(0)(_ + _ * numStates);
  }

  def decode(s: Int):ArrayBuffer[Int] = decode(s,window)
    
  def decode(s: Int, window: Int): ArrayBuffer[Int] = {
    val result = new ArrayBuffer[Int];
    var acc = s;
    while(acc != 0) {
      val r = acc % numStates;
      acc /= numStates;
      result += r;
    }
    while(result.size < window) {
      result += 0;
    }
    result;
  }

}
