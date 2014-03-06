package breeze.linalg

import operators._
import support._
import breeze.math.MutableInnerProductSpace
import breeze.math.Semiring
import DenseMatrix._

/** Import this to provide access to a DenseMatrix[Double] as a MutableInnerProductSpace, so it can be used in optimization. */
object MutableInnerProductSpaceDenseMatrixDouble {


//	//implicit val canDotD_f = new CanDotDDenseMatrix[Float]
//	//implicit val canDotD_i = new CanDotDDenseMatrix[Int]

	implicit val space_d = {
    class CanDotDDenseMatrix extends OpMulInner.Impl2[DenseMatrix[Double], DenseMatrix[Double], Double] {
   		override def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]):Double = {
   			require(a.rows == b.rows, "Vector row dimensions must match!")
   			require(a.cols == b.cols, "Vector col dimensions must match!")
   			val aVec = a.toDenseVector
   			val bVec = b.toDenseVector
   			aVec.dot(bVec)
   		}
   	}
    implicit val canDotD_d = new CanDotDDenseMatrix()
    MutableInnerProductSpace.make[DenseMatrix[Double], Double]
  }
}