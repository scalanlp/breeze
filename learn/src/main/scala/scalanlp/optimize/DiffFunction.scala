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
import scalala.tensor.operators._;
import TensorShapes._;

/**
* Represents a differentiable function.
*
* @author dlwh
*/
trait DiffFunction[K,T<:Tensor1[K]] extends (T=>Double) {
  /** calculates the gradient at a point */
  def gradientAt(x: T): T = calculate(x)._2;
  /** calculates the value at a point */
  def valueAt(x:T): Double = calculate(x)._1;

  def apply(x:T) = valueAt(x);

  /** Calculates both the value and the gradient at a point */
  def calculate(x:T):(Double,T);
}

object DiffFunction {
  def withL2Regularization[K,T<:TensorSelfOp[K,T,Shape1Col] with Tensor1[K]](d: DiffFunction[K,T],weight: Double) = new DiffFunction[K,T] {
    override def gradientAt(x:T):T = {
      val grad = d.gradientAt(x);
      myGrad(grad,x);
    }

    override def valueAt(x:T) = {
      val v = d.valueAt(x);
      v + myValueAt(x);
    }

    private def myValueAt(x:T) = {
      weight * math.pow(norm(x,2),2);
    }

    private def myGrad(g: T, x: T) = {
      val g2 = g.copy;
      g2 += (x * 2 * weight);
      g2
    }

    override def calculate(x: T) = {
      val (v,grad) = d.calculate(x);
      (v + myValueAt(x), myGrad(grad,x));
    }
  }

  // TODO: probably only want to shrink parameters that are "on"
  def withL2Regularization[K,T<:TensorSelfOp[K,T,Shape1Col] with Tensor1[K]](d: BatchDiffFunction[K,T],weight: Double):BatchDiffFunction[K,T] = new BatchDiffFunction[K,T] {
    override def gradientAt(x:T, batch: IndexedSeq[Int]):T = {
      val grad = d.gradientAt(x, batch);
      myGrad(grad,x, batch.size);
    }

    override def valueAt(x:T, batch: IndexedSeq[Int]) = {
      val v = d.valueAt(x, batch);
      v + myValueAt(x) * batch.size / fullRange.size;
    }

    private def myValueAt(x:T) = {
      weight * math.pow(norm(x,2),2);
    }

    private def myGrad(g: T, x: T, batchSize:Int) = {
      val g2 = g.copy;
      g2 += (x * 2 * weight * batchSize / d.fullRange.size);
      g2
    }

    override def calculate(x: T, batch: IndexedSeq[Int]) = {
      val (v,grad) = d.calculate(x, batch);
      (v + myValueAt(x), myGrad(grad,x, batch.size));
    }

    def fullRange = d.fullRange;
  }
}

/**
* A diff function that supports subsets of the data
*/
trait BatchDiffFunction[K,T<:Tensor1[K]] extends DiffFunction[K,T] with ((T,IndexedSeq[Int])=>Double) {
  /**
  * Calculates the gradient of the function on a subset of the data
  */
  def gradientAt(x:T, batch: IndexedSeq[Int]) : T = calculate(x,batch)._2;
  /**
  * Calculates the value of the function on a subset of the data
  */
  def valueAt(x:T, batch: IndexedSeq[Int]) : Double = calculate(x,batch)._1
  /**
  * Calculates the value and gradient of the function on a subset of the data;
  */
  def calculate(x:T, batch: IndexedSeq[Int]): (Double,T)

  override def calculate(x:T):(Double,T) = calculate(x,fullRange);
  override def valueAt(x:T):Double = valueAt(x,fullRange)
  override def gradientAt(x:T):T = gradientAt(x,fullRange)

  def apply(x:T, batch:IndexedSeq[Int]) = valueAt(x,batch);

  /**
  * The full size of the data
  */
  def fullRange: IndexedSeq[Int];
}
