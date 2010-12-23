package scalanlp.classify;

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

import math._;
import scalala.tensor.counters._;
import Counters._;
import scalanlp.data._;


import scala.collection.Map;

/** Implements a Naive-Bayes Classifer over bags of words.
 * It automatically trains itself given the collection c of
 * learning examples.
 *
 * @author dlwh
 * @param c: a collection of example documents
 * @param wordSmoothing: how much smoothing for each word
 * @param classSmoothing: how much smoothing for the class.
 */
@serializable
@SerialVersionUID(1L)
class NaiveBayes[L,W](c: Iterable[Example[L,IntCounter[W]]],
    val wordSmoothing:Double=0.05,
    val classSmoothing:Double=0.01)  extends Classifier[L,IntCounter[W]] {

  // p(c)
  private val classCounts = DoubleCounter[L]();
  // p(w|c)
  private val wordCounts = new PairedDoubleCounter[L,W]();

  private val vocabSize = {
    val myC = c;
    val allWords = scala.collection.mutable.Set[W]();
    for(e <- myC) {
       classCounts(e.label) += 1;
       for( (k,count) <- e.features)
	       wordCounts(e.label)(k) += count;
       allWords ++= e.features.keysIterator;
    }
    allWords.size;
  }

  /** Returns the unnormalized log probability of each class for the given document. */
  def scores(o : IntCounter[W]) = {
    val res = DoubleCounter[L]();
    for( (l,prior) <- classCounts) {
      res(l) += log(prior + classSmoothing);
      val probWC = wordCounts(l);
      val logDenom = log(probWC.total  + vocabSize * wordSmoothing);
      val logWordProbabilities = o.map{ (e:(W,Int)) => val (k,v) = e; v * (log(probWC(k) + wordSmoothing) - logDenom)}
      res(l) += logWordProbabilities.foldLeft(0.0)(_+_);
    }
    res;
  }
}

object NaiveBayes {
  class Trainer[L,T](wordSmoothing: Double=0.05, classSmoothing: Double= 0.01) extends Classifier.Trainer[L,IntCounter[T]] {
    type MyClassifier = NaiveBayes[L,T];

    override def train(data: Iterable[Example[L,IntCounter[T]]]) = {
      new NaiveBayes(data,wordSmoothing,classSmoothing);
    }
  }
}