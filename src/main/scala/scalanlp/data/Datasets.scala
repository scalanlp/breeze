package scalanlp.data;

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
import scalanlp.stats.sampling.Rand;

/**
* Provides useful utilties for dealing with datasets that have a defined order.
*
* @author dlwh
*/
object Datasets {
  /**
  * Split a training set into k-folds, with a test sets equal to
  * 1/kth of the data and training sets the rest of it. Returns a Seq of results,
  * one for each execution.
  *
  * Syntax: crossValidate(K, myDataSet)( (trainSet,testSet) =&gt; {produce a result} ) 
  */
  def crossValidate[T](k : Int, dataset: Seq[T]) = new {
    require(k < dataset.size);
    require(k > 0);
    def apply[R](f: (Seq[T], Seq[T])=>R): Seq[R] = {
      val chunkSize = dataset.size/k;
      val lastChunk = dataset.size % k;
      val result = new ArrayBuffer[R];
      if(lastChunk != 0) {
        result += f(dataset take (dataset.size - lastChunk), dataset drop (dataset.size - lastChunk) );
      }
      val remK = if(lastChunk == 0) k else k - 1;
      for(i <- 0 until remK) {
        val testSet = dataset drop (i * chunkSize) take (chunkSize);
        val trainSet = (dataset take (i * chunkSize)) ++ (dataset drop ( (i+1) * chunkSize))
          result += f(trainSet,testSet);
      }
      result
    }
  }

  /**
  * Leave-one-out Cross validation
  * Split a training set into dataset.size-folds, with a test sets equal to
  * 1 of the data and training sets for the rest of it. Returns a Seq of results,
  * one for each execution.
  *
  * This is probably very slow!
  *
  * Syntax: loocv(myDataSet)( (trainSet,testSet) => {produce a result} ) 
  */
  def loocv[T](dataset: Seq[T]) = new {
    def apply[R](f: (Seq[T], Seq[T])=>R): Seq[R] = {
      val result = new ArrayBuffer[R];
      for(i <- 0 until dataset.size) {
        val testSet = dataset drop (i) take 1;
        val trainSet = (dataset take i) ++ (dataset drop (i+1));
          result += f(trainSet,testSet);
      }
      result
    }
  }

  def randomize[T](dataset: Seq[T]) = {
    val perm = Rand.permutation(dataset.size).sample;
    (0 until dataset.size) map (perm) map (dataset);
  }
}
