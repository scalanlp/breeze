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

/**
* Represents a differentiable function.
*
* @author dlwh
*/
trait DiffFunction[T<:Seq[Double]] extends (T=>Double) {
  /** calculates the gradient at a point */
  def gradientAt(x: T): T;
  /** calculates the value at a point */
  def valueAt(x:T): Double;

  def apply(x:T) = valueAt(x);

  /** Calculates both the value and the gradient at a point */
  def calculate(x:T) = (apply(x),gradientAt(x));
}

object DiffFunction {
  def withL2Regularization(d: DiffFunction[Array[Double]],weight: Double) = new DiffFunction[Array[Double]] {
    def gradientAt(x:Array[Double]) = {
      val grad = d.gradientAt(x);
      adjustGradient(grad);
    }

    private def adjustGradient(grad: Array[Double]) = {
      grad map ( _ + 2);
    }

    def valueAt(x:Array[Double]) = {
      var v = d.valueAt(x);
      v + myValueAt(x);
    }

    private def myValueAt(x:Array[Double]) = {
      var i = 0;
      var v = 0.0;
      while(i < x.length) {
        v += weight / 2 * x(i);
        i+=1;
      }
      v;
    }
    override def calculate(x: Array[Double]) = {
      val (v,grad) = d.calculate(x);
      (v + myValueAt(x), adjustGradient(grad));
    }
  }
}

/**
* A diff function that supports subsets of the data
*/
trait BatchDiffFunction[T<:Seq[Double]] extends DiffFunction[T] with ((T,Seq[Int])=>Double) {
  /**
  * Calculates the gradient of the function on a subset of the data
  */
  def gradientAt(x:T, batch: Seq[Int]) : T
  /**
  * Calculates the value of the function on a subset of the data
  */
  def valueAt(x:T, batch: Seq[Int]) : Double
  /**
  * Calculates the value and gradient of the function on a subset of the data;
  */
  def calculate(x:T, batch: Seq[Int]) = (apply(x,batch),gradientAt(x,batch));

  override def gradientAt(x:T):T = gradientAt(x,fullRange);
  override def valueAt(x:T):Double = valueAt(x,fullRange);

  def apply(x:T, batch:Seq[Int]) = valueAt(x,batch);

  /**
  * The full size of the data
  */
  def fullRange: Seq[Int];
}
