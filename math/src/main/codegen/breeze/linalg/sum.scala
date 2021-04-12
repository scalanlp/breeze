package breeze.linalg

import breeze.generic.UFunc
import breeze.generic.UFunc.UImpl2
import breeze.linalg.operators.OpAdd
import breeze.macros.expand
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.math.Semiring
import breeze.storage.Zero

import scala.collection.TraversableOnce
import scala.reflect.ClassTag
import spire.syntax.cfor._
import scala.collection.compat._

object sum extends UFunc with sumLowPrio with VectorizedReduceUFunc {
  override type Op = OpAdd.type

  @expand
  implicit def reduce[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S]): Impl[T, S] =
    new Impl[T, S] {
      def apply(v: T): S = {
        class SumVisitor extends ValuesVisitor[S] {
          var sum: S = 0
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

  implicit def reduceSemiring[T, S](implicit iter: CanTraverseValues[T, S], semiring: Semiring[S]): Impl[T, S] =
    new Impl[T, S] {
      def apply(v: T): S = {
        class SumVisitor extends ValuesVisitor[S] {
          var sum: S = semiring.zero
          def visit(a: S): Unit = {
            sum = semiring.+(sum, a)
          }

          def zeros(numZero: Int, zeroValue: S): Unit = {}

        }
        val visit = new SumVisitor
        iter.traverse(v, visit)
        visit.sum
      }
    }

  @expand
  implicit def helper[@expand.args(Int, Float, Long, Double) T]: VectorizeHelper[T] = new VectorizeHelper[T] {
    override def zerosLike(len: Int): DenseVector[T] = DenseVector.zeros[T](len)

    override def combine(x: T, y: T): T = x + y
  }
}

/** Reducing UFunc that provides implementations for Broadcasted Dense stuff */
trait VectorizedReduceUFunc extends UFunc {
  type Op <: UFunc with Singleton

  trait VectorizeHelper[@specialized T] {
    def zerosLike(len: Int): DenseVector[T]
    def combine(x: T, y: T): T
  }

  implicit def vectorizeRows[T: ClassTag](
      implicit helper: VectorizeHelper[T],
      baseOp: UFunc.InPlaceImpl2[Op, DenseVector[T], DenseVector[T]])
    : Impl[BroadcastedRows[DenseMatrix[T], DenseVector[T]], DenseVector[T]] = {
    new Impl[BroadcastedRows[DenseMatrix[T], DenseVector[T]], DenseVector[T]] {
      override def apply(v: BroadcastedRows[DenseMatrix[T], DenseVector[T]]): DenseVector[T] = {
        val mat = v.underlying
        val result = helper.zerosLike(mat.rows)
        cforRange(0 until mat.cols) { i =>
          baseOp(result, mat(::, i))
        }
        result
      }
    }
  }

  implicit def vectorizeRows2[@specialized(Double, Float, Long, Int) T: ClassTag: Zero](implicit baseOp: Impl2[T, T, T])
    : Impl2[BroadcastedRows[DenseMatrix[T], DenseVector[T]], DenseVector[T], DenseMatrix[T]] = {
    new Impl2[BroadcastedRows[DenseMatrix[T], DenseVector[T]], DenseVector[T], DenseMatrix[T]] {
      override def apply(v: BroadcastedRows[DenseMatrix[T], DenseVector[T]], dv: DenseVector[T]): DenseMatrix[T] = {
        val mat = v.underlying
        require(dv.length == mat.cols, "Vector length must be same as number of columns!")
        if (!mat.isTranspose) {
          val res = DenseMatrix.zeros[T](mat.rows, mat.cols)
          cforRange(0 until mat.cols) { j =>
            val b = dv(j)
            cforRange(0 until mat.rows) { i =>
              res(i, j) = baseOp(mat(i, j), b)
            }
          }

          res
        } else {
          // starts out transposed
          val res = DenseMatrix.zeros[T](mat.cols, mat.rows).t
          cforRange2(0 until mat.rows, 0 until mat.cols) { (i, j) =>
            res(i, j) = baseOp(mat(i, j), dv(j))
          }
          res
        }
      }
    }
  }

  @expand
  implicit def vectorizeCols[@expand.args(Double, Float, Int, Long) T: ClassTag: Zero](
      implicit helper: VectorizeHelper[T])
    : Impl[BroadcastedColumns[DenseMatrix[T], DenseVector[T]], Transpose[DenseVector[T]]] = {
    new Impl[BroadcastedColumns[DenseMatrix[T], DenseVector[T]], Transpose[DenseVector[T]]] {
      override def apply(v: BroadcastedColumns[DenseMatrix[T], DenseVector[T]]): Transpose[DenseVector[T]] = {
        val mat = v.underlying
        val res = helper.zerosLike(mat.cols)

        if (!mat.isTranspose) {
          val d = mat.data
          cforRange(0 until mat.cols) { j =>
            var r = res(j)
            val baseOff = mat.offset + j * mat.majorStride
            cforRange(0 until mat.rows) { i =>
              r = helper.combine(r, d(baseOff + i))
            }
            res(j) = r
          }
        } else {
          cforRange(0 until mat.rows) { i =>
            cforRange(0 until mat.cols) { j =>
              res(j) = helper.combine(res(j), mat(i, j))
            }
          }
        }
        res.t
      }
    }
  }

  @expand
  implicit def vectorizeCols2[@expand.args(Double, Float, Int, Long) T: ClassTag: Zero](implicit impl2: Impl2[T, T, T])
    : Impl2[BroadcastedColumns[DenseMatrix[T], DenseVector[T]], DenseVector[T], DenseMatrix[T]] = {
    new Impl2[BroadcastedColumns[DenseMatrix[T], DenseVector[T]], DenseVector[T], DenseMatrix[T]] {
      override def apply(v: BroadcastedColumns[DenseMatrix[T], DenseVector[T]], dv: DenseVector[T]): DenseMatrix[T] = {
        val mat = v.underlying
        require(dv.length == mat.rows, "Vector length must be same as number of rows!")

        if (!mat.isTranspose) {
          val res = DenseMatrix.zeros[T](mat.rows, mat.cols)
          val d = mat.data
          val rd = res.data
          cforRange(0 until mat.cols) { j =>
            val baseOff = mat.offset + j * mat.majorStride
            val rBaseOff = j * mat.majorStride
            cforRange(0 until mat.rows) { i =>
              rd(rBaseOff + i) = impl2(d(baseOff + i), dv(i))
            }
          }
          res
        } else {
          // note this starts out row major (aka isTranspose) so it has the same shape as the input
          val res = DenseMatrix.zeros[T](mat.cols, mat.rows).t
          cforRange(0 until mat.rows) { i =>
            val cmp = dv(i)
            cforRange(0 until mat.cols) { j =>
              res(i, j) = impl2(mat(i, j), cmp)
            }
          }
          // we fix it here
          res.t.copy
        }
      }
    }
  }

}

sealed trait sumLowPrio { this: sum.type =>
  implicit def sumSummableThings[CC, T](implicit view: CC <:< Iterable[T], tSum: OpAdd.Impl2[T, T, T]): Impl[CC, T] = {
    new Impl[CC, T] {
      override def apply(v: CC): T = v.reduceLeft(tSum(_, _))
    }
  }

  implicit def sumIterator[T](implicit tSum: OpAdd.Impl2[T, T, T]): Impl[Iterator[T], T] = {
    new Impl[Iterator[T], T] {
      override def apply(v: Iterator[T]): T = v.reduce(tSum(_, _))
    }
  }
}
