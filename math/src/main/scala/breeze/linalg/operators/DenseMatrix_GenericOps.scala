package breeze.linalg.operators

import breeze.linalg._
import breeze.math.Semiring
import breeze.util.ReflectionUtil
import scalaxy.debug.require

import scala.reflect.ClassTag

trait DenseMatrix_GenericOps extends MatrixOps {

  implicit def impl_OpMulMatrix_DM_DM_eq_DM_Generic[T: Semiring]
  : OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] =
    new OpMulMatrix.Impl2[DenseMatrix[T], DenseMatrix[T], DenseMatrix[T]] {
      implicit val ring: Semiring[T] = implicitly[Semiring[T]]
      override def apply(a: DenseMatrix[T], b: DenseMatrix[T]): DenseMatrix[T] = {
        implicit val ct: ClassTag[T] = ReflectionUtil.elemClassTagFromArray(a.data)

        val res: DenseMatrix[T] = DenseMatrix.zeros[T](a.rows, b.cols)
        require(a.cols == b.rows)

        val colsB = b.cols
        val colsA = a.cols
        val rowsA = a.rows

        var j = 0
        while (j < colsB) {
          var l = 0;
          while (l < colsA) {

            val v = b(l, j)
            var i = 0
            while (i < rowsA) {
              res(i, j) = ring.+(res(i, j), ring.*(a(i, l), v))
              i += 1
            }
            l += 1
          }
          j += 1
        }
        res
      }
    }


  implicit def impl_OpMulMatrix_DM_V_eq_DV_Generic[T, Vec<:Vector[T]](
                                                        implicit ring: Semiring[T]): OpMulMatrix.Impl2[DenseMatrix[T], Vec, DenseVector[T]] =
      (a: DenseMatrix[T], b: Vec) => {
        implicit val ct: ClassTag[T] = ReflectionUtil.elemClassTagFromArray(a.data)
        require(a.cols == b.length)
        val res: DenseVector[T] = DenseVector.zeros[T](a.rows)
        var c = 0
        while (c < a.cols) {
          var r = 0
          while (r < a.rows) {
            val v = a(r, c)
            res(r) = ring.+(res(r), ring.*(v, b(c)))
            r += 1
          }
          c += 1
        }

        res
      }

}
