package scalanlp.optimize;

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

import scalala._;
import scalala.Scalala._;
import scalala.tensor._;

/**
* Represents a differentiable function.
*
* @author dlwh
*/
trait DiffFunction[K,T<:Tensor1[K]] extends (T=>Double) {
  /** calculates the gradient at a point */
  def gradientAt(x: T): T;
  /** calculates the value at a point */
  def valueAt(x:T): Double;

  def apply(x:T) = valueAt(x);

  /** Calculates both the value and the gradient at a point */
  def calculate(x:T) = (apply(x),gradientAt(x));
}

object DiffFunction {
  def withL2Regularization[K,T<:Tensor1[K]](d: DiffFunction[K,T],weight: Double) = new DiffFunction[K,T] {
    def gradientAt(x:T):T = {
      val grad = d.gradientAt(x);
      adjustGradient(grad);
    }

    private def adjustGradient(grad: T) = {
      grad += 2
      grad;
    }

    def valueAt(x:T) = {
      var v = d.valueAt(x);
      v + myValueAt(x);
    }

    private def myValueAt(x:T) = {
      sum( x * weight / 2 value);
    }

    override def calculate(x: T) = {
      val (v,grad) = d.calculate(x);
      (v + myValueAt(x), adjustGradient(grad));
    }
  }
}

/**
* A diff function that supports subsets of the data
*/
trait BatchDiffFunction[K,T<:Tensor1[K]] extends DiffFunction[K,T] with ((T,Seq[K])=>Double) {
  /**
  * Calculates the gradient of the function on a subset of the data
  */
  def gradientAt(x:T, batch: Seq[K]) : T
  /**
  * Calculates the value of the function on a subset of the data
  */
  def valueAt(x:T, batch: Seq[K]) : Double
  /**
  * Calculates the value and gradient of the function on a subset of the data;
  */
  def calculate(x:T, batch: Seq[K]) = (apply(x,batch),gradientAt(x,batch));

  override def gradientAt(x:T):T = gradientAt(x,fullRange);
  override def valueAt(x:T):Double = valueAt(x,fullRange);

  def apply(x:T, batch:Seq[K]) = valueAt(x,batch);

  /**
  * The full size of the data
  */
  def fullRange: Seq[K];
}
