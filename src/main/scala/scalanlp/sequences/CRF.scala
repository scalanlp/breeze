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
import stats.sampling._

/**
* Represents a single-markov CRF, can score sequences and such.
*
* @param features, functions of the form (previousState,nextState,i,obs)=&lt;Double
* @param weights, to go with the features
* @param startState: an initial state, that seq's implicitly start with.
*/
class CRF[S,T](val features: Seq[(S,S,Int,Seq[T])=>Double],
               val weights: Seq[Double], val stateDict: Set[S]) {
  require(features.length == weights.length);
  val logWeights = weights.map(Math.log _).elements.collect;

  /** Finds the Viterbi parse for the given sequence. (The maximal parse) 
  final def viterbi(obs: Seq[T]) = {
    val bestPaths = (0 until obs.length).foldLeft( List( (List(startState),0.0)) ) { 
      (candidates:List[(List[S],Double)], i) =>
      val m = scala.collection.mutable.Map[S,(List[S],Double)]();
      for {
        (path,score) <- candidates;
        prevState = path.head;
        state <- stateDict
      } {
        val fVals = features.elements.map(_(prevState,nextState,i,obs))
        val newScore = fVals.map(Math log _).zip(logWeights.elements).foldLeft(0.0)(_+_);
        if(!m.contains(state) || m(state)._2 < newScore + score) {
          m(state) = (state::path,newScore + score)
        }
      }
      m.values.toList;
    }
    
    bestPaths.reduceLeft( (a,b) => if (a._2 > b._2) a else b)._1.reverse.tail
  }
  */
}  
