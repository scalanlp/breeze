package breeze.optimize

import breeze.generic.UFunc
import breeze.generic.UFunc.UImpl2
import breeze.linalg.NumericOps
import breeze.linalg.operators.{OpAdd, OpDiv, OpMulMatrix, OpSub}
import breeze.linalg.support.CanCopy
import breeze.math.InnerProductModule
import breeze.util.Isomorphism

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

/**
 * Represents a differentiable function whose output is guaranteed to be consistent
 *
 * @author dlwh
 */
trait DiffFunction[T] extends StochasticDiffFunction[T] with NumericOps[DiffFunction[T]] { outer =>

  override def repr: DiffFunction[T] = this

  def cached(implicit copy: CanCopy[T]) = {
    if (this.isInstanceOf[CachedDiffFunction[_]]) {
      this
    } else {
      new CachedDiffFunction(this)
    }
  }

  override def throughLens[U](implicit l: Isomorphism[T, U]): DiffFunction[U] =
    new DiffFunction[U] {
      override def calculate(u: U) = {
        val t = l.backward(u)
        val (obj, gu) = outer.calculate(t)
        (obj, l.forward(gu))
      }
    }
}

object DiffFunction extends DiffFunctionOpImplicits {
  def withL2Regularization[T, I](d: DiffFunction[T], weight: Double)(implicit space: InnerProductModule[T, Double]) =
    new DiffFunction[T] {
      import space._
      override def gradientAt(x: T): T = {
        val grad = d.gradientAt(x)
        myGrad(grad, x)
      }

      override def valueAt(x: T) = {
        val v = d.valueAt(x)
        myValueAt(v, x)
      }

      private def myValueAt(v: Double, x: T) = {
        v + weight * (x.dot(x)) / 2
      }

      private def myGrad(g: T, x: T): T = {
        g + (x * weight)
      }

      override def calculate(x: T) = {
        val (v, grad) = d.calculate(x)
        (myValueAt(v, x), myGrad(grad, x))
      }
    }

  def withL2Regularization[T, I](d: BatchDiffFunction[T], weight: Double)(implicit
      space: InnerProductModule[T, Double]
  ): BatchDiffFunction[T] =
    new BatchDiffFunction[T] {
      import space._
      override def gradientAt(x: T, batch: IndexedSeq[Int]): T = {
        val grad = d.gradientAt(x, batch)
        myGrad(grad, x)
      }

      override def valueAt(x: T, batch: IndexedSeq[Int]) = {
        val v = d.valueAt(x, batch)
        v + myValueAt(x)
      }

      private def myValueAt(x: T) = {
        weight * (x.dot(x)) / 2
      }

      private def myGrad(g: T, x: T) = {
        g + (x * weight)
      }

      override def calculate(x: T, batch: IndexedSeq[Int]) = {
        val (v, grad) = d.calculate(x, batch)
        (v + myValueAt(x), myGrad(grad, x))
      }

      def fullRange = d.fullRange
    }
}

sealed trait DiffFunctionOpImplicits { this: DiffFunction.type =>

  implicit def opAddDiffFunction[T](implicit
      opAdd: OpAdd.Impl2[T, T, T]
  ): OpAdd.Impl2[DiffFunction[T], DiffFunction[T], DiffFunction[T]] = {
    new OpAdd.Impl2[DiffFunction[T], DiffFunction[T], DiffFunction[T]] {
      override def apply(f: DiffFunction[T], f2: DiffFunction[T]): DiffFunction[T] = {
        new DiffFunction[T] {
          override def calculate(x: T): (Double, T) = {
            val (v1, g1) = f.calculate(x)
            val (v2, g2) = f2.calculate(x)

            (v1 + v2, opAdd(g1, g2))
          }
        }

      }
    }
  }

  implicit def opSubDiffFunction[T](implicit
      opSub: OpSub.Impl2[T, T, T]
  ): OpSub.Impl2[DiffFunction[T], DiffFunction[T], DiffFunction[T]] = {
    new OpSub.Impl2[DiffFunction[T], DiffFunction[T], DiffFunction[T]] {
      override def apply(f: DiffFunction[T], f2: DiffFunction[T]): DiffFunction[T] = {
        new DiffFunction[T] {
          override def calculate(x: T): (Double, T) = {
            val (v1, g1) = f.calculate(x)
            val (v2, g2) = f2.calculate(x)

            (v1 - v2, opSub(g1, g2))
          }
        }

      }
    }
  }

  implicit def opMulDiffFunction[T](implicit
      opMul: OpMulMatrix.Impl2[T, Double, T]
  ): OpMulMatrix.Impl2[DiffFunction[T], Double, DiffFunction[T]] = {
    new OpMulMatrix.Impl2[DiffFunction[T], Double, DiffFunction[T]] {
      override def apply(f: DiffFunction[T], v: Double): DiffFunction[T] = {
        new DiffFunction[T] {
          override def calculate(x: T): (Double, T) = {
            val (v1, g1) = f.calculate(x)

            (v1 * v, opMul(g1, v))
          }
        }

      }
    }
  }

  implicit def opMulLHSDiffFunction[T](implicit
      opMul: OpMulMatrix.Impl2[Double, T, T]
  ): OpMulMatrix.Impl2[Double, DiffFunction[T], DiffFunction[T]] = {
    new OpMulMatrix.Impl2[Double, DiffFunction[T], DiffFunction[T]] {
      override def apply(v: Double, f: DiffFunction[T]): DiffFunction[T] = {
        new DiffFunction[T] {
          override def calculate(x: T): (Double, T) = {
            val (v1, g1) = f.calculate(x)

            (v1 * v, opMul(v, g1))
          }
        }

      }
    }
  }

  implicit def opDivDiffFunction[T](implicit
      opDiv: OpDiv.Impl2[T, Double, T]
  ): OpDiv.Impl2[DiffFunction[T], Double, DiffFunction[T]] = {
    new OpDiv.Impl2[DiffFunction[T], Double, DiffFunction[T]] {
      override def apply(f: DiffFunction[T], v: Double): DiffFunction[T] = {
        new DiffFunction[T] {
          override def calculate(x: T): (Double, T) = {
            val (v1, g1) = f.calculate(x)

            (v1 / v, opDiv(g1, v))
          }
        }

      }
    }
  }

  // d/dx a/f(x) = -a * f'(x) / f(x)^2
  implicit def opDivLHSDiffFunction[T](implicit
      opMul: OpMulMatrix.Impl2[Double, T, T]
  ): OpDiv.Impl2[Double, DiffFunction[T], DiffFunction[T]] = {
    new OpDiv.Impl2[Double, DiffFunction[T], DiffFunction[T]] {
      override def apply(v: Double, f: DiffFunction[T]): DiffFunction[T] = {
        new DiffFunction[T] {
          override def calculate(x: T): (Double, T) = {
            val (v1, g1) = f.calculate(x)

            (v / v1, opMul(-v / (v1 * v1), g1))
          }
        }

      }
    }
  }

  implicit def castOps[V1, V2, T, Op, VR](implicit
      v1ev: V1 <:< DiffFunction[T],
      V2ev: V2 <:< DiffFunction[T],
      op: UImpl2[Op, DiffFunction[T], DiffFunction[T], VR]
  ): UImpl2[Op, V1, V2, VR] = {
    op.asInstanceOf[UFunc.UImpl2[Op, V1, V2, VR]]
  }

}
