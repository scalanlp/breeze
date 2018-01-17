package breeze.linalg

import breeze.generic.UFunc

/**
 * mirrors the columns (left<->right).
 *
 * @author dlwh
 */
object fliplr extends UFunc {

  implicit def implDM[T]: Impl[DenseMatrix[T], DenseMatrix[T]] = new Impl[DenseMatrix[T], DenseMatrix[T]] {
    def apply(v: DenseMatrix[T]): DenseMatrix[T] = {
      if (!v.isTranspose) {
        v(::, v.cols - 1 to 0 by -1).copy
      } else { // we can't deal with strides on the minor axis, so, copy to make it column major
        apply(v.copy)
      }
    }
  }

}

/**
 * mirrors the rows (up down)
 *
 * @author dlwh
 */
object flipud extends UFunc {

  implicit def implDM[T]: Impl[DenseMatrix[T], DenseMatrix[T]] = new Impl[DenseMatrix[T], DenseMatrix[T]] {
    def apply(v: DenseMatrix[T]): DenseMatrix[T] = {
      if (v.isTranspose) {
        v(v.cols - 1 to 0 by -1, ::).copy
      } else { // we can't deal with strides on the minor axis, so, copy to make it column major
        fliplr(v.t).t.copy
      }
    }
  }

  implicit def implDV[T]: Impl[DenseVector[T], DenseVector[T]] = new Impl[DenseVector[T], DenseVector[T]] {
    def apply(v: DenseVector[T]): DenseVector[T] = {
      v(v.length - 1 to 0 by -1).copy
    }
  }

}

/**
 * Rotates a matrix by 90 * k degrees counter clockwise. if k is not specified, it defaults to 1.
 *
 * @author dlwh
 */
object rot90 extends UFunc {

  implicit def implDM[T]: Impl2[DenseMatrix[T], Int, DenseMatrix[T]] = new Impl2[DenseMatrix[T], Int, DenseMatrix[T]] {
    def apply(v: DenseMatrix[T], k: Int): DenseMatrix[T] = {
      (k % 4) match {
        case 0 => v.copy
        case 1 | -3 => fliplr(v).t
        case 2 | -2 => fliplr(flipud(v))
        case 3 | -1 => fliplr(v.t)
        case _ => sys.error("Shouldn't be here!")
      }
    }
  }

  implicit def impl1fromImpl2[T, R](implicit impl2: Impl2[T, Int, R]): Impl[T, R] = new Impl[T, R] {
    def apply(v: T): R = impl2(v, 1)
  }
}
