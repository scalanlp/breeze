package breeze.linalg

import breeze.generic.UFunc
import scala.reflect.ClassTag
import spire.implicits._
import breeze.storage.Zero

/**
 * split the array
 *
 * @author stucchio
 */
object split extends UFunc {

  implicit def implIntVec[T: ClassTag]: Impl2[DenseVector[T], Int, IndexedSeq[DenseVector[T]]] = {
    new Impl2[DenseVector[T], Int, IndexedSeq[DenseVector[T]]] {
      def apply(v: DenseVector[T], n: Int): IndexedSeq[DenseVector[T]] = {
        require(n >= 0)
        require(n <= v.size)
        require(v.size % n == 0)

        val individualVectorSize = v.size / n
        val result = new collection.mutable.ArrayBuffer[DenseVector[T]]()

        cforRange(0 until n) { k =>
          val offsetInOriginalVector = k*individualVectorSize
          val chunk = new Array[T](individualVectorSize)
          cforRange(0 until individualVectorSize){i =>
            chunk(i) = v(offsetInOriginalVector+i)
          }
          result += DenseVector[T](chunk)
        }
        result
      }
    }
  }

  implicit def implSeqVec[T: ClassTag]: Impl2[DenseVector[T], Seq[Int], IndexedSeq[DenseVector[T]]] = new Impl2[DenseVector[T], Seq[Int], IndexedSeq[DenseVector[T]]] {
    def apply(v: DenseVector[T], nSeq: Seq[Int]): IndexedSeq[DenseVector[T]] = {
      require(nSeq.size < v.size)

      val result = new collection.mutable.ArrayBuffer[DenseVector[T]]()
      var lastN: Int = 0
      nSeq.foreach{ n =>
        val chunk = new Array[T](n - lastN)
        cforRange(lastN until n) { i =>
          chunk(i-lastN) = v(i)
        }
        result += DenseVector[T](chunk)
        lastN = n
      }
      if (lastN < v.size) { //If we did not already add last chunk to result, do it now.
        val chunk = new Array[T](v.size - lastN)
        cforRange(lastN until v.size) { i =>
          chunk(i-lastN) = v(i)
        }
        result += DenseVector[T](chunk)
      }
      result
    }
  }

  implicit def implIntMatrix[T: ClassTag](implicit zero: Zero[T]): Impl3[DenseMatrix[T], Int, Int, IndexedSeq[DenseMatrix[T]]] = new Impl3[DenseMatrix[T], Int, Int, IndexedSeq[DenseMatrix[T]]] {
    def apply(v: DenseMatrix[T], n: Int, axis: Int): IndexedSeq[DenseMatrix[T]] = axis match {
      case 0 => vsplit(v,n)
      case 1 => hsplit(v,n)
      case _ => throw new IllegalArgumentException("Matrices have only two axes.")
    }
  }
}

object hsplit extends UFunc {
  implicit def implIntVec[T: ClassTag]: Impl2[DenseVector[T], Int, IndexedSeq[DenseVector[T]]] = new Impl2[DenseVector[T], Int, IndexedSeq[DenseVector[T]]] { //For vectors just an alias
    def apply(v: DenseVector[T], n: Int): IndexedSeq[DenseVector[T]] = hsplit(v,n)
  }

  implicit def implSeqVec[T: ClassTag]: Impl2[DenseVector[T], Seq[Int], IndexedSeq[DenseVector[T]]] = new Impl2[DenseVector[T], Seq[Int], IndexedSeq[DenseVector[T]]] { //For vectors just an alias
    def apply(v: DenseVector[T], n: Seq[Int]): IndexedSeq[DenseVector[T]] = hsplit(v,n)
  }

  implicit def implIntMat[T: ClassTag](implicit zero: Zero[T]): Impl2[DenseMatrix[T], Int, IndexedSeq[DenseMatrix[T]]] = new Impl2[DenseMatrix[T],Int, IndexedSeq[DenseMatrix[T]]] { //for matrices
    def apply(v: DenseMatrix[T], n: Int): IndexedSeq[DenseMatrix[T]] = {
      require(n >= 0)
      require(n <= v.cols)
      require(v.cols % n == 0)

      val result = new collection.mutable.ArrayBuffer[DenseMatrix[T]]()
      val newCols = v.cols / n
      val newSize = v.rows * newCols

      cforRange(0 until n) { k =>
        val offsetInOriginalMatrix = k*newCols
        val chunk = DenseMatrix.create(v.rows, newCols, new Array[T](newSize))
        cforRange2(0 until v.rows, 0 until newCols) { (i, j) =>
          chunk(i,j) = v(i,j+offsetInOriginalMatrix)
        }
        result += chunk
      }
      result
    }
  }
}

object vsplit extends UFunc {
  implicit def implIntMat[T: ClassTag](implicit zero: Zero[T]): Impl2[DenseMatrix[T], Int, IndexedSeq[DenseMatrix[T]]] = new Impl2[DenseMatrix[T],Int, IndexedSeq[DenseMatrix[T]]] { //for matrices
    def apply(v: DenseMatrix[T], n: Int): IndexedSeq[DenseMatrix[T]] = {
      require(n >= 0)
      require(n <= v.cols)
      require(v.cols % n == 0)

      val result = new collection.mutable.ArrayBuffer[DenseMatrix[T]]()
      val newRows = v.rows / n

      cforRange(0 until n) { k =>
        val offsetInOriginalMatrix = k*newRows
        val chunk = DenseMatrix.create(newRows, v.cols, new Array[T](v.cols * newRows))
        cforRange2(0 until newRows, 0 until v.cols) { (i, j) =>
          chunk(i,j) = v(i+offsetInOriginalMatrix,j)
        }
        result += chunk
      }
      result
    }
  }
}
