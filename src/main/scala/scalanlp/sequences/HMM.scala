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


import counters._;
import math.Numerics._;
import util.Implicits._;
import stats.sampling._;
import Math._;

/**
* Represents a single-markov HMM, can score sequences and such.
*
* @param transitions, must be normalized, p(t_i|t_{i+1})
* @param emissions, must be normalize, p(e_i|t_i)
* @param startState: an initial state, that seq's implicitly start with.
*/
class HMM[S,T](val transitions: PairedDoubleCounter[S,S], val emissions: PairedDoubleCounter[S,T], val startState: S) extends Measure[Seq[T]] {
  require(transitions.map(_._2).forall(c => c.total =~= 1.0 || c.total =~= 0.0));
  require(emissions.map(_._2).forall(c => c.total =~= 1.0 || c.total =~= 0.0));

  /** Finds the Viterbi parse for the given sequence. (The maximal parse) */
  final def viterbi(obs: Seq[T]) = {
    val bestPaths = obs.foldLeft( List( (List(startState),0.0)) ) { (candidates:List[(List[S],Double)], o) =>
      val m = scala.collection.mutable.Map[S,(List[S],Double)]();
      for {
        (path,score) <- candidates;
        prevState = path.head;
        (state,pTrans) <- transitions(prevState);
        if pTrans > 0
        pEm = emissions(state,o)
        if pEm > 0
      } {
        val newScore = score + Math.log(pTrans) + Math.log(pEm);
        if(!m.contains(state) || m(state)._2 < newScore) {
          m(state) = (state::path,newScore)
        }
      }
      m.values.toList;
    }
    
    bestPaths.reduceLeft( (a,b) => if (a._2 > b._2) a else b)._1.reverse.tail
  }
  
  /** calculates the forward logprobability for this sequence. */
  def forward(obs: Seq[T]): Seq[DoubleCounter[S]] = {
    val init = DoubleCounter[S]();
    init(startState) = 0.0;
    
    obs.foldLeft(init :: Nil) { (probs,o) =>
      val prev = probs.head;
      val next = DoubleCounter[S]();
      for{ (prevState,aP) <- prev;
           (nextState,pTrans) <- transitions(prevState);
           if pTrans > 0
           val pEm = emissions(nextState,o)
           if pEm > 0
      } /*do*/ {
        val update = aP + Math.log(pTrans) + Math.log(pEm);
        if(!next.contains(nextState)) {
          next(nextState) = update; 
        } else {
          next(nextState) = logSum(next(nextState),update);
        }
      }
      next :: probs;
    }.reverse.tail // get rid of initial state
  }

  def probabilityOf(obs: Seq[T]): Double = exp(logProbabilityOf(obs));
  def logProbabilityOf(obs: Seq[T]): Double = (forward(obs).last.total)
  def apply(obs: Seq[T]) = exp(logApply(obs)); 
  override def logApply(obs: Seq[T]) = logProbabilityOf(obs);
  
  private lazy val reverseProbabilities = {
    val inverted = new PairedDoubleCounter[S,S];
    inverted += (for( (a,b,p) <- transitions.triples) yield {
      (b,a,p)
    })
    inverted
  }
  
  /** calculates the backward logprobability for this sequence. */
  def backward(obs: Seq[T]): Seq[DoubleCounter[S]] = {
    val tail = DoubleCounter[S]();
    tail(startState) = 0.0;
    
    obs.reverse.foldLeft(tail :: Nil) { (probs,o) =>
      val prev = probs.head;
      val next = DoubleCounter[S]();
      for{ (nextState,bP) <- prev;
           (prevState,pTrans) <- reverseProbabilities(nextState);
           if pTrans > 0
           val pEm = emissions(nextState,o)
           if pEm > 0
         } {
         val update = bP + Math.log(pTrans) + Math.log(pEm);
         if(!next.contains(nextState)) {
           next(prevState) = update; 
         } else {
           next(prevState) = logSum(next(prevState),update);
         }
      }
      next :: probs;
    }
  } 
  
  /** Returns log p(x_i=t|obs) for each i*/
  def posteriorProbabilities(obs: Seq[T]): Seq[DoubleCounter[S]] = {
    val f = forward(obs);
    val b = backward(obs);
    (for(i <- 0 until obs.length) yield {
      val result = DoubleCounter[S];
      for(k <- f(i).keys if b.contains(k)) {
        result(k) = f(i)(k);
      }
      for(k <- b(i).keys if f.contains(k)) {
        result(k) += b(i)(k);
      }
      result;
    }).toList
  }
  
  /** Returns argmax_t p(x_i=t|obs) for each i. 
   * That is, the most likely assigment for each position
   */
  def decodePosterior(obs: Seq[T]) = posteriorProbabilities(obs).map(_.argmax)
  
}

object HMM {
  /** Trains an HMM from a set of observed sequences, adding smoothing
  * pseudocounts to each transition.
  */
  def train[S,T](seqs: Seq[Seq[(S,T)]], startState: S, smoothing: Double):HMM[S,T] = {
    val trans = new PairedDoubleCounter[S,S];
    val emit = new PairedDoubleCounter[S,T];
    for { 
      seq <- seqs;
      i <- 0 until seq.length
    } {
      if(i == 0) {
        trans(startState).incrementCount(seq(i)._1,1);
      } else {
        trans(seq(i)._1).incrementCount(seq(i+1)._1,1);
      } 
      emit(seq(i)._1).incrementCount(seq(i)._2,1);
    }
    // handle the smoothing
    for(s <- emit.keys) {
      if(!emit.contains(startState))
        trans(startState)(s) += smoothing;
      for(s2 <- emit.keys) {
        trans(s)(s2) += smoothing;
      }
    }
    trans.transform{ (k,c) => c.normalized} 
    emit.transform{ (k,c) => c.normalized} 
    new HMM(trans,emit,startState);
  }

  /** Trains an HMM from a set of observed sequences */
  def train[S,T](seqs: Seq[Seq[(S,T)]], startState: S):HMM[S,T] = train(seqs, startState, 0.0);
}
