package breeze.optimize

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

import scalala.generic.math.CanNorm
import scalala.library.Library.norm
import scalala.operators.{NumericOps, OpAdd, OpMul, BinaryOp}
import scalala.operators.bundles.VectorSpace

/**
* Represents a differentiable function whose output is guaranteed to be consistent
*
* @author dlwh
*/
trait DiffFunction[T] extends StochasticDiffFunction[T]

object DiffFunction {
  def withL2Regularization[T](d: DiffFunction[T],weight: Double)
                             (implicit canNorm: CanNorm[T],vspace: VectorSpace[Double,T]) = new DiffFunction[T] {
    import vspace._
    override def gradientAt(x:T):T = {
      val grad = d.gradientAt(x)
      myGrad(grad,x)
    }

    override def valueAt(x:T) = {
      val v = d.valueAt(x)
      myValueAt(v, x)
    }

    private def myValueAt(v: Double, x:T) = {
      v + weight * math.pow(norm(x,2),2) / 2
    }

    private def myGrad(g: T, x: T):T = {
      g + (x * weight)
    }

    override def calculate(x: T) = {
      val (v,grad) = d.calculate(x)
      (myValueAt(v, x), myGrad(grad,x))
    }
  }

  def withL2Regularization[T](d: BatchDiffFunction[T],weight: Double)
                             (implicit canNorm: CanNorm[T],
                              view: T => NumericOps[T],
                              addVector: BinaryOp[T,T,OpAdd,T],
                              mulScalar: BinaryOp[T,Double,OpMul,T]):BatchDiffFunction[T] = new BatchDiffFunction[T] {
    override def gradientAt(x:T, batch: IndexedSeq[Int]):T = {
      val grad = d.gradientAt(x, batch)
      myGrad(grad,x)
    }

    override def valueAt(x:T, batch: IndexedSeq[Int]) = {
      val v = d.valueAt(x, batch)
      v + myValueAt(x)
    }

    private def myValueAt(x:T) = {
      weight * math.pow(norm(x,2),2) / 2
    }

    private def myGrad(g: T, x: T) = {
      g + (x * weight)
    }

    override def calculate(x: T, batch: IndexedSeq[Int]) = {
      val (v,grad) = d.calculate(x, batch)
      (v + myValueAt(x), myGrad(grad,x))
    }

    def fullRange = d.fullRange
  }
}


