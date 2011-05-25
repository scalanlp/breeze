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
import scalala.tensor.mutable._;
import scalanlp.data._;
import scalala.tensor.::



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
class NaiveBayes[L,W](c: Iterable[Example[L,Counter[W,Double]]],
    val wordSmoothing:Double=0.05,
    val classSmoothing:Double=0.01)  extends Classifier[L,Counter[W,Double]] {


  private val (wordCounts:Counter2[L,W,Double],classCounts: Counter[L,Double], vocabSize: Int) =  {
    // numYes(c)
    val classCounts = Counter[L,Double]();
    // numYes(w|c)
    val wordCounts = Counter2[L,W,Double]();
    val myC = c;
    val allWords = scala.collection.mutable.Set[W]();
    for(e <- myC) {
       classCounts(e.label) += 1;
       for( k <- e.features.domain)
	       wordCounts(e.label,k) += e.features(k);
       allWords ++= e.features.data.keys;
    }
    val vocabSize = allWords.size;
    classCounts :+= classSmoothing;
    (wordCounts,classCounts,vocabSize)
  }

  val wordTotals = { for(k <- classCounts.keysIterator) yield k -> wordCounts(k,::).sum} toMap

  /** Returns the unnormalized log probability of each class for the given document. */
  def scores(o : Counter[W,Double]) = {
    val res = Counter[L,Double]();
    for( l <- classCounts.domain) {
      val prior = classCounts(l);
      res(l) += log(prior + classSmoothing);
      val probWC = wordCounts(l,::)
      val logDenom = log(wordTotals(l) + vocabSize * wordSmoothing);
      val logWordProbabilities = o.pairsIterator.map { case (k,v) =>  v * (log(probWC(k) + wordSmoothing) - logDenom)}
      res(l) += logWordProbabilities.sum;
    }
    res;
  }
}

object NaiveBayes {
  class Trainer[L,T](wordSmoothing: Double=0.05, classSmoothing: Double= 0.01) extends Classifier.Trainer[L,Counter[T,Double]] {
    type MyClassifier = NaiveBayes[L,T];

    override def train(data: Iterable[Example[L,Counter[T,Double]]]) = {
      new NaiveBayes(data,wordSmoothing,classSmoothing);
    }
  }
}
