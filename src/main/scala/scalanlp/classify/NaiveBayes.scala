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

import Math._;
import scalanlp.counters._;
import scalanlp.data._;


import scala.collection.Map;

/** Implements a Naive-Bayes Classifer over bags of words.
 * It automatically trains itself given the collection c of
 * learning examples.
 * 
 * @param wordSmoothing: how much smoothing for each
 */
class NaiveBayes[W,L](c: =>Collection[Example[L,Map[W,Int]]],
    val wordSmoothing:Double,
    val classSmoothing:Double)  extends Classifier[L,Map[W,Int]] {

  // p(c)
  private val classCounts = IntCounter[L]();
  // p(w|c)
  private val wordCounts = new PairedIntCounter[L,W]();

  // numWords
  private val vocabSize = train();

  private def train() = {
    val myC = c;
    val allWords = scala.collection.mutable.Set[W]();
    for(e <- myC) {
       classCounts(e.label) += 1;
       wordCounts(e.label) ++= e.features;
       allWords ++= e.features.keys;
    }
    allWords.size;
  }

  /** Returns the unnormalized log probability of each class for the given document. */
  def scores(o : Observation[Map[W,Int]]) = {
    val res = DoubleCounter[L]();
    for( (l,prior) <- classCounts) {
      res(l) += log(prior + classSmoothing);
      val probWC = wordCounts(l);
      val logDenom = log(probWC.total  + vocabSize * wordSmoothing);
      val logWordProbabilities = o.features.map{ case (k,v) => v * (log(probWC(k) + wordSmoothing) - logDenom)}
      res(l) += logWordProbabilities.foldLeft(0.0)(_+_);
    }
    res;
  }
}

/**
 * Main program to run NaiveBayes, more a demo than anything else.
 * 
 * Syntax: RunNaiveBayes <train directory> <test directory>
 * 
 * where the directories are organized such that there are
 * L subdirectories (one per label) and all files in each dir
 * have that label. Prints per-class stats and runs a 
 * randomization test to see if changing smoothing makes a difference.
 */
object RunNaiveBayes {
  def main(args : Array[String]) {
    import java.io._;
    import scalanlp.stats._;
    
    
    val trainData = 
      for( dir <- new File(args(0)).listFiles;
      file <- dir.listFiles)
    yield Bag.fromFile(file);

    val testData = for( dir <- new File(args(1)).listFiles;
      file <- dir.listFiles)
    yield Bag.fromFile(file);

    val nb = new NaiveBayes(trainData,3,0.1);

    val trainStats = ContingencyStats(nb,trainData);
    println("Train");
    println(trainStats);

    val testStats = ContingencyStats(nb,testData);
    println("Test");
    println(testStats);

    val nb2 = new NaiveBayes(trainData,0.1,0.1);

    val trainStats2 = ContingencyStats(nb2,trainData);
    println("Train2");
    println(trainStats2);

    println(RandomizationTest(trainData,nb,nb2));
  }
}
