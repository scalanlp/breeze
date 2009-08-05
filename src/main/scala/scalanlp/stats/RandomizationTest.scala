package scalanlp.stats;

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


import scala.collection.mutable._;
import scalanlp.data._;
import scalanlp.classify._;
import sampling._;

/** Implements statistical significance testing for the output of two systems by randomization. 
* This system assumes they're on the same dataset, which changes the procedure.
* Follows Teh, 2000 More accurate tests for the statistical significance of result differences.
*
* Labels must have .equals.
*
* @author dlwh
*/
// TODO: use quasi-random bit sequence.
class RandomizationTest[L](val numSamples:Int, val errorMeasure: Seq[L]=>Double) extends ((Seq[L],Seq[L])=>Double) {
  def this(errorMeasure: Seq[L]=>Double) = this(5000,errorMeasure);

  def diff(l1: Seq[L], l2: Seq[L]) = Math.abs( errorMeasure(l1) - errorMeasure(l2));
  def apply(labeling1: Seq[L], labeling2: Seq[L]) = {
    assume(labeling1.length == labeling2.length);
    // git rid of any overlapping labels
    val lpairs = (labeling1.iterator zip labeling2.iterator).filter( a => a._1 != a._2).toSequence;
    val baseDiff = diff(lpairs.view.map(_._1),lpairs.view.map(_._2));
    var numBetter = 0;
    for(i <- 1 to numSamples) {
      val l1 = new ArrayBuffer[L]();
      val l2 = new ArrayBuffer[L]();
      for( (a,b) <- lpairs) {
        if(Rand.uniform.get < .5) {
          l1 += a;
          l2 += b;
        } else {
          l1 += b;
          l2 += a;
        }
      }
      if(baseDiff < diff(l1,l2)) {
        numBetter += 1;
      }
    }
    (numBetter + 1.0) / (numSamples+1.0);
  }
}

object RandomizationTest {

  /** Classify the dataset according to the two classifiers, and then use some
  * measure from ContingencyStats to see if they're different.
  */
  def apply[L,T](dataset: Seq[Example[L,T]], c1: Classifier[L,T], c2: Classifier[L,T], error: ContingencyStats[L]=>Double):Double = {
    new RandomizationTest[L]( l1=> 
      error(ContingencyStats(l1,dataset.map(_.label)))
    ) apply (dataset.view.map(Example.lift(c1)).map(_.label).force,dataset.view.map(Example.lift(c2)).map(_.features).force);
  }

  /** Classify the dataset according to the two classifiers, and then use f1
  * measure from ContingencyStats to see if they're different.
  */
  def apply[L,T](dataset: Seq[Example[L,T]], c1: Classifier[L,T], c2: Classifier[L,T]):Double = {
    apply(dataset,c1,c2,(x:ContingencyStats[L]) => x.macroaveraged.f);
  }

}
