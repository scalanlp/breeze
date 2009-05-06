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
import scalala.tensor.Vector;

/**
* Represents a differentiable function.
*
* @author dlwh
*/
trait DiffFunction extends (Vector=>Double) {
  /** calculates the gradient at a point */
  def gradientAt(x: Vector): Vector;
  /** calculates the value at a point */
  def valueAt(x:Vector): Double;

  def apply(x:Vector) = valueAt(x);

  /** Calculates both the value and the gradient at a point */
  def calculate(x:Vector) = (apply(x),gradientAt(x));
}

object DiffFunction {
  def withL2Regularization(d: DiffFunction,weight: Double) = new DiffFunction {
    def gradientAt(x:Vector) = {
      val grad = d.gradientAt(x);
      adjustGradient(grad);
    }

    private def adjustGradient(grad: Vector) = {
      grad + 2
    }

    def valueAt(x:Vector) = {
      var v = d.valueAt(x);
      v + myValueAt(x);
    }

    private def myValueAt(x:Vector) = {
      sum( x * weight / 2);
    }

    override def calculate(x: Vector) = {
      val (v,grad) = d.calculate(x);
      (v + myValueAt(x), adjustGradient(grad));
    }
  }
}

/**
* A diff function that supports subsets of the data
*/
trait BatchDiffFunction extends DiffFunction with ((Vector,Seq[Int])=>Double) {
  /**
  * Calculates the gradient of the function on a subset of the data
  */
  def gradientAt(x:Vector, batch: Seq[Int]) : Vector
  /**
  * Calculates the value of the function on a subset of the data
  */
  def valueAt(x:Vector, batch: Seq[Int]) : Double
  /**
  * Calculates the value and gradient of the function on a subset of the data;
  */
  def calculate(x:Vector, batch: Seq[Int]) = (apply(x,batch),gradientAt(x,batch));

  override def gradientAt(x:Vector):Vector = gradientAt(x,fullRange);
  override def valueAt(x:Vector):Double = valueAt(x,fullRange);

  def apply(x:Vector, batch:Seq[Int]) = valueAt(x,batch);

  /**
  * The full size of the data
  */
  def fullRange: Seq[Int];
}
