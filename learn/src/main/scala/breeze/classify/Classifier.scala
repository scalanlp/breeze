package breeze.classify;

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


import breeze.data._
import scalala.tensor.mutable.Counter
import scalala.tensor.Tensor1

/**
 * Represents a classifier from observations of type T to labels of type L.
 * Implementers should only need to implement score.
 * 
 * @author dlwh
 */
trait Classifier[L,-T] extends (T=>L) { outer =>
  /** Return the most likely label */
  def apply(o :T) = classify(o);
  /** Return the most likely label */
  def classify(o :T) = scores(o).argmax;

  /** For the observation, return the score for each label that has a nonzero 
   *  score. 
   */
  def scores(o: T): Tensor1[L,Double];

  /**
   * Transforms output labels L=>M. if f(x) is not one-to-one then the max of score
   * from the L's are used.
   */
  def map[M](f: L=>M):Classifier[M,T] = new Classifier[M,T] {
    def scores(o: T): Counter[M,Double] = {
      val ctr = Counter[M,Double]();
      val otherCtr = outer.scores(o);
      for( x <- otherCtr.domain) {
        val y = f(x);
        ctr(y) = ctr(y) max otherCtr(x);
      }
      ctr;
    }
  }
}

object Classifier {
  trait Trainer[L,T] {
    type MyClassifier <: Classifier[L,T]
    def train(data: Iterable[Example[L,T]]):MyClassifier;
  }
}
