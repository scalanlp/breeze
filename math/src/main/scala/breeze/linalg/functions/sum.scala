package breeze.linalg

import breeze.generic.UFunc
import breeze.linalg.operators.OpAdd
import breeze.macros.expand
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.math.Semiring
import breeze.storage.Zero
import com.sun.istack.internal.Pool.Impl

import scala.collection.TraversableOnce
import scala.reflect.ClassTag
import spire.syntax.cfor._

object sum extends UFunc with sumLowPrio with VectorizedReduceUFunc {
  override type Op = OpAdd.type

  @expand
  implicit def reduce[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl[T, S] = new Impl[T, S] {
    def apply(v: T): S = {
      class SumVisitor extends ValuesVisitor[S] {
        var sum : S = 0
        def visit(a: S): Unit = {
          sum += a
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
          sum += numZero * zeroValue
        }
      }
      val visit = new SumVisitor
      iter.traverse(v, visit)
      visit.sum
    }
  }

  implicit def reduceSemiring[T, S](implicit iter: CanTraverseValues[T, S], semiring: Semiring[S]): Impl[T, S] = new Impl[T, S] {
    def apply(v: T): S = {
      class SumVisitor extends ValuesVisitor[S] {
        var sum : S = semiring.zero
        def visit(a: S): Unit = {
          sum = semiring.+(sum, a)
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
        }

      }
      val visit = new SumVisitor
      iter.traverse(v, visit)
      visit.sum
    }
  }

  @expand
  implicit def helper[@expand.args(Int, Float, Long, Double) T]:VectorizeHelper[T] = new VectorizeHelper[T] {
    override def zerosLike(len: Int): DenseVector[T] = DenseVector.zeros[T](len)

    override def combine(x: T, y: T): T = x + y
  }
}

/** Reducing UFunc that provides implementations for Broadcasted Dense stuff */
trait VectorizedReduceUFunc extends UFunc {
  type Op <: UFunc with Singleton

  trait VectorizeHelper[@specialized T] {
    def zerosLike(len: Int):DenseVector[T]
    def combine(x: T, y: T):T
  }

  implicit def vectorizeRows[T:ClassTag](implicit helper: VectorizeHelper[T],
                                         baseOp: UFunc.InPlaceImpl2[Op, DenseVector[T], DenseVector[T]]): Impl[BroadcastedRows[DenseMatrix[T], DenseVector[T]], DenseVector[T]] = {
    new Impl[BroadcastedRows[DenseMatrix[T], DenseVector[T]], DenseVector[T]] {
      override def apply(v: BroadcastedRows[DenseMatrix[T], DenseVector[T]]): DenseVector[T] = {
        val mat = v.underlying
        val result = helper.zerosLike(mat.rows)
        cforRange(0 until mat.cols) { i =>
          baseOp(result, mat(::, i))
        }
//        helper.finish(result)
        result
      }
    }
  }

  @expand
  implicit def vectorizeCols[@expand.args(Double, Float, Int, Long) T:ClassTag:Zero](implicit helper: VectorizeHelper[T]): Impl[BroadcastedColumns[DenseMatrix[T], DenseVector[T]], Transpose[DenseVector[T]]] = {
    new Impl[BroadcastedColumns[DenseMatrix[T], DenseVector[T]], Transpose[DenseVector[T]]] {
      override def apply(v: BroadcastedColumns[DenseMatrix[T], DenseVector[T]]): Transpose[DenseVector[T]] = {
        val mat = v.underlying
        val res = helper.zerosLike(mat.cols)

        if (!mat.isTranspose) {
          val d = mat.data
          cforRange(0 until mat.cols) { i =>
            var r = res(i)
            val baseOff = mat.offset + i * mat.rows
            cforRange(0 until mat.rows) { j =>
              r = helper.combine(r, d(baseOff + j))
            }
            res(i) = r
          }
        } else {
          cforRange(0 until mat.cols) { i =>
            var r = res(i)
            cforRange(0 until mat.rows) { j =>
              r = helper.combine(r, mat(j, i))
            }
            res(i) = r
          }
        }
        res.t
      }
    }
  }

}

sealed trait sumLowPrio { this: sum.type =>
  implicit def sumSummableThings[CC, T](implicit view: CC <:< TraversableOnce[T], tSum: OpAdd.Impl2[T, T, T]):Impl[CC, T] = {
    new Impl[CC, T] {
      override def apply(v: CC): T = v.reduceLeft(tSum(_, _))
    }
  }
}
