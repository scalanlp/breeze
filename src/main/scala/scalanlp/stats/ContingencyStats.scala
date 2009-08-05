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

import scalanlp.classify.Classifier;
import scalanlp.data._;

import ContingencyStats._;
import DescriptiveStats._;

/** Provides precision, recall and f-score for labellings.
* @author dlwh
*/
class ContingencyStats[L] private (private val classWise: Map[L,Table]) {
  def this() = this(Map[L,Table]().withDefaultValue(Table(0,0,0)));

  /**
   * Add an observation.
   */
  def +(l :(L,L)) = apply(l._1,l._2);

  /** Takes a guess and a gold standard set of labelings. */
  def apply(guessgold: (L,L)):ContingencyStats[L] = this(guessgold._1,guessgold._2);

  /** Takes a guess and a gold standard set of labelings. */
  def apply(guess: Set[L], gold: Set[L]):ContingencyStats[L] = {
    val tps = (guess intersect gold).foldLeft(classWise) { (m,l) => m + (l -> m(l).incTP)};
    val fps = (guess -- gold).foldLeft(tps) { (m,l) => m + (l -> m(l).incFP)};
    val fns = (gold -- guess).foldLeft(fps) { (m,l) => m + (l -> m(l).incFN)};
    new ContingencyStats(fns)
  }


  def apply(guess: L, gold: L): ContingencyStats[L] = {
    if(guess == gold) {
      val tbl = classWise(guess);
      new ContingencyStats[L](classWise + (guess->tbl.incTP))
    } else {
      val tbl = classWise(guess);
      val tblL = classWise(gold);
      new ContingencyStats[L](classWise + (guess->tbl.incFP) + (gold->tblL.incFN));
    }
  }

  def precision(l:L) = classWise(l).precision;
  def recall(l:L) = classWise(l).recall;
  /** F1 score for this label. */
  def f(l:L) = classWise(l).f;
  def f(l:L,beta:Double) = classWise(l).f(beta);

  lazy val microaveraged = new {
    private val tbl = classWise.valuesIterator.foldLeft(Table(0,0,0))(_+_);
    val precision = tbl.precision;
    val recall = tbl.recall;
    val f = tbl.f;
    def f(beta:Double) = tbl f beta;
  }

  lazy val macroaveraged = new {
    val precision = mean(classWise.valuesIterator.map(_.precision))
    val recall = mean(classWise.valuesIterator.map(_.recall))
    val f = mean(classWise.valuesIterator.map(_.f))
    def f(beta:Double) = mean(classWise.valuesIterator.map(_.f(beta)))
  }

  private def r(x:Double) = "%.4f" format x;

  override def toString() = {
    val buf = new StringBuilder;
    buf ++= "Contingency Statistics:\n";
    buf ++= "==========================\n";
    buf ++= "Macro: Prec " + r(macroaveraged.precision) + " Recall: " + r(macroaveraged.recall) + " F1: " + r(macroaveraged.f) + "\n";
    buf ++= "Micro: Prec " + r(microaveraged.precision) + " Recall: " + r(microaveraged.recall) + " F1: " + r(microaveraged.f) + "\n";
    buf ++= "==========================\n";

    for( (l,tbl) <- classWise) {
      buf ++= l + ": Prec " + r(tbl.precision) + " Recall: " + r(tbl.recall) + " F1: " + r(tbl.f) + "\n";
    }
    buf.toString;
  }

}

object ContingencyStats {
  def apply[L]():ContingencyStats[L] = new ContingencyStats[L];

  /**
  * Classify every example and compute its statistics
  */
  def apply[L,T](classifier: Classifier[L,T], dataset: Seq[Example[L,T]]):ContingencyStats[L] = {
    apply(dataset.map(Example.lift(classifier)) map (_.features),dataset.map(_.label));
  }

  def apply[L](guessed: Seq[L], gold: Seq[L]):ContingencyStats[L] = {
    require(guessed.length == gold.length);
    (guessed.iterator zip gold.iterator).foldLeft(ContingencyStats[L]())(_+_);
  }

  class Accuracy(val numRight: Int, val numTotal: Int) {
    def this() = this(0,0)
    def accuracy = if(numTotal == 0) 0. else numRight.asInstanceOf[Double]/ numTotal;
    def + (b:Boolean) = new Accuracy(if(b) numRight + 1 else numRight, numTotal + 1);
    def ++(b:Iterator[Boolean]) = b.foldLeft(this)(_+_);
    def ++(b:Iterable[Boolean]) = b.foldLeft(this)(_+_);

    override def toString = "Accuracy: " + accuracy;
  }

  // true positive, false positive, false negative. TN is only used
  // in Accuracy, which is unreliable and requires access to the label
  // set.
  private[stats] case class Table(tp: Int, fp:Int, fn: Int) {
    def incTP = Table(tp+1,fp,fn);
    def incFP = Table(tp,fp+1,fn);
    def incFN = Table(tp,fp,fn+1);

    def +(t :Table) = {
      val Table(otp, ofp, ofn) = t;
      Table(tp + otp, fp + ofp, fn + ofn);
    }

    def precision = { 
      val denom = tp + fp;
      if(denom == 0) 0.0
      else tp * 1.0 / denom;
    }

    def recall = { 
      val denom = tp + fn;
      if(denom == 0) 1.0
      else tp * 1.0 / denom;
    }


    /** F-Score
    */
    def f:Double = f(1.);

    /** F-\beta = (1+beta^2) * (precision * recall) / (\beta^2 * pr + re) 
    */
    def f(beta: Double) = {
      val pr = precision;
      val re = recall;
      (1 + beta * beta) * (pr * re) / (beta * beta * pr + re);
    }

  }
}

