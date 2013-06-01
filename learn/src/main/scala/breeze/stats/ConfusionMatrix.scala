package breeze.stats

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


import ConfusionMatrix._
import breeze.classify.Classifier
import chalk.data._

/** Provides a string representation of common errors from a classifier.
* @author dlwh
*/
class ConfusionMatrix[L] private (private val classWise: Map[L,Map[L,Int]]) {
  def this() = this(Map[L,Map[L,Int]]().withDefaultValue(Map[L,Int]().withDefaultValue(0)))

  /**
   * Add an observation to the confusion matrix. The pair is a guess label and a gold label
   */
  def +(l :(L,L)) = {
    add(l._1,l._2)
  }

  /**
   * Add an observation to the confusion matrix. The pair is a guess label and a gold label
   */
  def add(guess: L, gold: L) = {
    var tbl = classWise(gold)
    tbl += (guess -> (tbl(guess) + 1))
    new ConfusionMatrix[L](classWise + (gold->tbl))
  }

  override def toString() = {
    val canonical = (Set()  ++ classWise.valuesIterator.flatMap(_.keysIterator)).iterator.toSeq
    val buf = new StringBuilder
    buf ++= "======================================================\n"
    buf ++= canonical.mkString("\t","\t","\n")
    buf ++= "======================================================\n"

    for( l <- canonical) {
      val tbl = classWise(l)
      buf ++= l.toString
      for(l2 <- canonical) {
        buf ++= "\t"
        buf ++= ""+tbl(l2) 
      }
      buf ++= "\n"
    }
    buf ++= "======================================================\n"
    buf.toString
  }

}

object ConfusionMatrix {
  def apply[L]():ConfusionMatrix[L] = new ConfusionMatrix[L]
}
