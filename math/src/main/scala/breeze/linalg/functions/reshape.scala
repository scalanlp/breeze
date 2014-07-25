package breeze.linalg

import breeze.generic.UFunc
import breeze.math.Semiring
import breeze.storage.Zero
import scala.reflect.ClassTag

/**
 * breeze
 * 7/4/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 * sv < 1, 2, 3, 4, 5, 6, 7, 8, 9>
 * csc [ [ 1 2 3 ]
 *       [ 4 5 6 ] ]
 * cP = < 0 2 4 6 >
 *
 * nsc [ [ 1 5 ]
 *       [ 4 3 ]
 *       [ 2 6 ] ]
 * ncP = < 0 3 6 >
 *
 * csc.data < 1, 4, 7, 2, 5, 8, 3, 6, 9 >
 *
 */
object reshape extends UFunc {
  implicit def svReshape[T:ClassTag:Semiring:Zero]: Impl3[SparseVector[T],Int,Int,CSCMatrix[T]] =
    new Impl3[SparseVector[T],Int,Int,CSCMatrix[T]] {
      def apply(sv: SparseVector[T],rows: Int, cols: Int): CSCMatrix[T] = {
        var i = 0
        val cscB = new CSCMatrix.Builder[T](rows,cols,sv.activeSize)
        while (i < sv.activeSize) {
          cscB.add(sv.index(i) / cols,sv.index(i) % cols,sv.data(i))
          i += 1
        }
        cscB.result(true,false)
      }
    }

  implicit def dvReshape[T:ClassTag:Semiring:Zero]: Impl3[DenseVector[T],Int,Int,DenseMatrix[T]] =
    new Impl3[DenseVector[T],Int,Int,DenseMatrix[T]] {
      def apply(v: DenseVector[T], rows: Int, cols: Int): DenseMatrix[T] = {
        require(v.length == rows * cols, "Vector length must equal rows * cols to reshape.")
        new DenseMatrix[T](rows,cols,v.toArray)
      }
    }

  implicit def dmReshape[T:ClassTag:Semiring:Zero]: Impl3[DenseMatrix[T],Int,Int,DenseMatrix[T]] =
    new Impl3[DenseMatrix[T],Int,Int,DenseMatrix[T]] {
      def apply(dm: DenseMatrix[T], rows: Int, cols: Int): DenseMatrix[T] = {
        require(dm.rows * dm.cols == rows * cols,
          "Cannot reshape a (%d,%d) matrix to a (%d,%d) matrix!".format(dm.rows, dm.cols, rows, cols))
        val nDM = new DenseMatrix[T](dm.rows,dm.cols,new Array[T](dm.activeSize))
        // in-place set method should be used to take advantage of blas.dcopy for T = Double
        // Unsure how blas.dcopy compares to System.arraycopy, which could also be used
        nDM := dm
        new DenseMatrix(rows, cols, nDM.data, dm.offset, if(dm.isTranspose) cols else rows, dm.isTranspose)
      }
    }

  implicit def cscReshape[T:ClassTag:Semiring:Zero]: Impl3[CSCMatrix[T],Int,Int,CSCMatrix[T]] =
    new Impl3[CSCMatrix[T],Int,Int,CSCMatrix[T]] {
      def apply(csc: CSCMatrix[T], rows: Int, cols: Int): CSCMatrix[T] = {
        require(csc.rows * csc.cols == rows * cols, "Size of matrix must match new dimensions (i.e. m.rows * m.cols == rows * cols")
        // Copy data
        val nData = new Array[T](csc.activeSize)
        System.arraycopy(csc.data,0,nData,0,csc.activeSize)

        // calculate colPtrs and rowIndices
        val rIndex = new Array[Int](csc.activeSize)
        val cPtrs = new Array[Int](cols + 1)
        cPtrs(0) = 0
        var nColInd = 1
        var lastCol = 0
        var c = 0
        while (c < csc.cols) {
          var ip = csc.colPtrs(c)
          while (ip < csc.colPtrs(c + 1)) {
            val r = csc.rowIndices(ip)
            // project into vector index
            val dld = c * csc.rows + r
            // project back into new (row,col) indices
            rIndex(ip) = dld % rows
            val nCol = dld / rows
            // fill out column pointers from last column to this one
            if (nCol > lastCol) {
              while (nColInd <= nCol) {
                cPtrs(nColInd) = ip
                nColInd += 1
              }
              lastCol = nCol
            }
              ip += 1
          }
          c += 1
        }
        while (nColInd < cPtrs.length) {
          cPtrs(nColInd) = nData.length
          nColInd += 1
        }
        new CSCMatrix[T](nData,rows,cols,cPtrs,csc.activeSize,rIndex)
      }
    }
}
