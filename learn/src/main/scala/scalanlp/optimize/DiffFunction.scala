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

import scalala.generic.math.CanNorm
import scalala.library.Library.norm
import scalala.operators.{NumericOps, OpAdd, OpMul, BinaryOp}
import scalala.operators.bundles.VectorSpace
import scalanlp.util.Lens
;

/**
* Represents a differentiable function.
*
* @author dlwh
*/
trait DiffFunction[T] extends (T=>Double) { outer =>
  /** calculates the gradient at a point */
  def gradientAt(x: T): T = calculate(x)._2;
  /** calculates the value at a point */
  def valueAt(x:T): Double = calculate(x)._1;

  def apply(x:T) = valueAt(x);

  /** Calculates both the value and the gradient at a point */
  def calculate(x:T):(Double,T);

  /**
   * Lenses provide a way of mapping between two types, which we typically
   * use to convert something to a DenseVector or other Tensor for optimization purposes.
   */
  def throughLens[U](implicit l: Lens[T,U]) = new DiffFunction[U] {
    def calculate(u: U) = {
      val t = l.backward(u);
      val (obj,gu) = outer.calculate(t);
      (obj,l.forward(gu));
    }
  }

}

object DiffFunction {
  def withL2Regularization[T](d: DiffFunction[T],weight: Double)
                             (implicit canNorm: CanNorm[T],vspace: VectorSpace[Double,T]) = new DiffFunction[T] {
    import vspace._;
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
      g + (x * (2 * weight))
    }

    override def calculate(x: T) = {
      val (v,grad) = d.calculate(x);
      (v + myValueAt(x), myGrad(grad,x));
    }
  }

  def withL2Regularization[T](d: BatchDiffFunction[T],weight: Double)
                             (implicit canNorm: CanNorm[T],
                              view: T <%< NumericOps[T],
                              addVector: BinaryOp[T,T,OpAdd,T],
                              mulScalar: BinaryOp[T,Double,OpMul,T]):BatchDiffFunction[T] = new BatchDiffFunction[T] {
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

    private def myGrad(g: T, x: T, batchSize: Int) = {
      g + (x * (2 * weight * batchSize / d.fullRange.size));
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
trait BatchDiffFunction[T] extends DiffFunction[T] with ((T,IndexedSeq[Int])=>Double) {
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
