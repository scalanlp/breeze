package breeze.linalg.operators

import breeze.linalg.support.CanMapValues.DenseCanMapValues
import breeze.linalg.{DenseMatrix, DenseVector, HashVector, SparseVector, Vector}
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support._
import breeze.linalg.support.CanZipAndTraverseValues.PairValuesVisitor
import breeze.macros.{cforRange, cforRange2}
import breeze.math.Complex
import breeze.storage.Zero

import scala.reflect.ClassTag

trait Vector_TraversalOps {

  // TODO There's a bizarre error specializing float's here.
  class CanZipMapValuesVector[@specialized(Int, Double) V, @specialized(Int, Double) RV: ClassTag]
    extends CanZipMapValues[Vector[V], V, RV, Vector[RV]] {
    def create(length: Int) = DenseVector(new Array[RV](length))

    def map(from: Vector[V], from2: Vector[V], fn: (V, V) => RV) = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      cforRange (0 until from.length) { i =>
        result.data(i) = fn(from(i), from2(i))
      }
      result
    }
  }

  // TODO: probably should have just made this virtual and not a typeclass??
  implicit def canMapValues_V[V, V2: Zero](implicit man: ClassTag[V2]): CanMapValues[Vector[V], V, V2, Vector[V2]] = {
    new CanMapValues[Vector[V], V, V2, Vector[V2]] {
      def map(from: Vector[V], fn: (V) => V2): Vector[V2] = from match {
        case sv: SparseVector[V] => sv.mapValues(fn)(SparseVector.canMapValues)
        case hv: HashVector[V] => hv.mapValues(fn)
        case dv: DenseVector[V] => dv.mapValues(fn)
        case _ => DenseVector.tabulate(from.length)(i => fn(from(i)))
      }

      def mapActive(from: Vector[V], fn: (V) => V2): Vector[V2] = from match {
        case sv: SparseVector[V] => sv.mapActiveValues(fn)
        case hv: HashVector[V] => hv.mapActiveValues(fn)
        case dv: DenseVector[V] => dv.mapActiveValues(fn)
        case _ => DenseVector.tabulate(from.length)(i => fn(from(i)))
      }
    }
  }

  implicit def canZipMapValues_V[V, R: ClassTag]: CanZipMapValuesVector[V, R] = new CanZipMapValuesVector[V, R]

  class CanZipMapKeyValuesVector[@specialized(Double, Int, Float, Long) V, @specialized(Int, Double) RV: ClassTag]
    extends CanZipMapKeyValues[Vector[V], Int, V, RV, Vector[RV]] {
    def create(length: Int) = DenseVector(new Array[RV](length))

    def map(from: Vector[V], from2: Vector[V], fn: (Int, V, V) => RV): Vector[RV] = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result.data(i) = fn(i, from(i), from2(i))
        i += 1
      }
      result
    }

    override def mapActive(from: Vector[V], from2: Vector[V], fn: (Int, V, V) => RV): Vector[RV] = {
      map(from, from2, fn)
    }
  }

  implicit def zipMapKV_V[V, R: ClassTag]: CanZipMapKeyValuesVector[V, R] = new CanZipMapKeyValuesVector[V, R]

  implicit def canIterateValues_V[V]: CanTraverseValues[Vector[V], V] = new CanTraverseValues[Vector[V], V] {

    def isTraversableAgain(from: Vector[V]): Boolean = true

    def traverse(from: Vector[V], fn: ValuesVisitor[V]): fn.type = {
      for (v <- from.valuesIterator) {
        fn.visit(v)
      }
      fn
    }

  }

  implicit def canTraverseKeyValuePairs_V[V]: CanTraverseKeyValuePairs[Vector[V], Int, V] =
    new CanTraverseKeyValuePairs[Vector[V], Int, V] {
      def isTraversableAgain(from: Vector[V]): Boolean = true

      def traverse(from: Vector[V], fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[Int, V]): Unit = {
        for (i <- 0 until from.length)
          fn.visit(i, from(i))
      }

    }
}

trait DenseVector_TraversalOps extends Vector_TraversalOps {

  implicit def DV_canIterateValues[V]: CanTraverseValues[DenseVector[V], V] =
    new CanTraverseValues[DenseVector[V], V] {

      def isTraversableAgain(from: DenseVector[V]): Boolean = true

      def traverse(from: DenseVector[V], fn: ValuesVisitor[V]): fn.type = {
        fn.visitArray(from.data, from.offset, from.length, from.stride)
        fn
      }

    }

  implicit def DV_canTraverseZipValues[V, W]: CanZipAndTraverseValues[DenseVector[V], DenseVector[W], V, W] =
    new CanZipAndTraverseValues[DenseVector[V], DenseVector[W], V, W] {

      def traverse(from1: DenseVector[V], from2: DenseVector[W], fn: PairValuesVisitor[V, W]): Unit = {
        if (from1.size != from2.size) {
          throw new IllegalArgumentException("Vectors to be zipped must have same size")
        }
        cforRange(0 until from1.size) { i =>
          fn.visit(from1(i), from2(i))
        }
      }
    }

  implicit def DV_canTraverseKeyValuePairs[V]: CanTraverseKeyValuePairs[DenseVector[V], Int, V] =
    new CanTraverseKeyValuePairs[DenseVector[V], Int, V] {
      def isTraversableAgain(from: DenseVector[V]): Boolean = true

      def traverse(from: DenseVector[V], fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[Int, V]): Unit = {
        import from._

        fn.visitArray((ind: Int) => (ind - offset) / stride, data, offset, length, stride)
      }

    }

  implicit def DV_canTransformValues[@specialized(Int, Float, Double) V]: CanTransformValues[DenseVector[V], V] =
    new CanTransformValues[DenseVector[V], V] {
      def transform(from: DenseVector[V], fn: (V) => V): Unit = {
        val data = from.data
        val length = from.length
        val stride = from.stride

        val offset = from.offset
        if (stride == 1) {
          cforRange(offset until offset + length) { j =>
            data(j) = fn(data(j))
          }
        } else {
          slowPath(fn, data, length, stride, offset)
        }
      }

      private def slowPath(fn: (V) => V, data: Array[V], length: Int, stride: Int, offset: Int): Unit = {
        val end = offset + stride * length
        var j = offset
        while (j != end) {
          data(j) = fn(data(j))
          j += stride
        }
      }

      def transformActive(from: DenseVector[V], fn: (V) => V): Unit = {
        transform(from, fn)
      }
    }

  implicit def canMapPairs[V, V2](
                                   implicit man: ClassTag[V2]): CanMapKeyValuePairs[DenseVector[V], Int, V, V2, DenseVector[V2]] =
    new CanMapKeyValuePairs[DenseVector[V], Int, V, V2, DenseVector[V2]] {

      /**Maps all key-value pairs from the given collection. */
      def map(from: DenseVector[V], fn: (Int, V) => V2): DenseVector[V2] = {
        // slow: DenseVector.tabulate(from.length)(i => fn(i, from(i)))
        val arr = new Array[V2](from.length)

        val d = from.data
        val stride = from.stride

        var i = 0
        var j = from.offset
        while (i < arr.length) {
          arr(i) = fn(i, d(j))
          i += 1
          j += stride
        }
        DenseVector[V2](arr)
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: DenseVector[V], fn: (Int, V) => V2): DenseVector[V2] = {
        map(from, fn)
      }
    }

}

trait SparseVector_TraversalOps extends Vector_TraversalOps {
  implicit def canIterateValues_SV[V]: CanTraverseValues[SparseVector[V], V] = {
    new CanTraverseValues[SparseVector[V], V] {

      def isTraversableAgain(from: SparseVector[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: SparseVector[V], fn: ValuesVisitor[V]): fn.type = {
        fn.zeros(from.size - from.activeSize, from.default)
        fn.visitArray(from.data, 0, from.activeSize, 1)
        fn
      }
    }
  }
}


trait DenseMatrix_TraversalOps extends TensorLowPrio {

  implicit def canTraverseValues[V]: CanTraverseValues[DenseMatrix[V], V] = {
    new CanTraverseValues[DenseMatrix[V], V] {
      def isTraversableAgain(from: DenseMatrix[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: DenseMatrix[V], fn: ValuesVisitor[V]): fn.type = {
        import from._
        val idealMajorStride = if (isTranspose) cols else rows

        if (majorStride == idealMajorStride) {
          fn.visitArray(data, offset, rows * cols, 1)
        } else {
          cforRange (0 until from.majorSize) { j =>
            fn.visitArray(data, offset + j * majorStride, minorSize, 1)
          }
        }
        //        else {
        //          var j = 0
        //          while (j < from.cols) {
        //            var i = 0
        //            while (i < from.rows) {
        //              fn.visit(from(i, j))
        //              i += 1
        //            }
        //            j += 1
        //          }
        //        }
        fn
      }

    }
  }

  implicit def canTraverseKeyValuePairs[V]: CanTraverseKeyValuePairs[DenseMatrix[V], (Int, Int), V] = {
    new CanTraverseKeyValuePairs[DenseMatrix[V], (Int, Int), V] {
      def isTraversableAgain(from: DenseMatrix[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: DenseMatrix[V], fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[(Int, Int), V]): Unit = {
        import from._
        val idealMajorStride = if (isTranspose) cols else rows

        if (majorStride == idealMajorStride) {
          fn.visitArray(from.rowColumnFromLinearIndex, data, offset, rows * cols, 1)
        } else if (!from.isTranspose) {
          // TODO: make this work for isTranspose too
          cforRange (0 until from.cols) { j =>
            fn.visitArray(from.rowColumnFromLinearIndex, data, offset + j * majorStride, rows, 1)
          }
        } else {
          cforRange2 (0 until from.rows, 0 until from.cols) { (i, j) =>
            fn.visit((i, j), from(i, j))
          }
        }
      }

    }
  }

  implicit def canTransformValues_DM[@specialized(Int, Float, Double) V]: CanTransformValues[DenseMatrix[V], V] = {
    new CanTransformValues[DenseMatrix[V], V] {
      def transform(from: DenseMatrix[V], fn: (V) => V): Unit = {
        if (from.isContiguous) {
          val d = from.data
          cforRange(from.offset until from.offset + from.size) { j =>
            d(j) = fn(d(j))
          }
        } else {
          slowPath(from, fn)
        }
      }

      private def slowPath(from: DenseMatrix[V], fn: (V) => V): Unit = {
        var off = from.offset
        val d = from.data
        cforRange (0 until from.majorSize) { big =>
          cforRange (off until off + from.minorSize) { pos =>
            d(pos) = fn(d(pos))
          }
          off += from.majorStride
        }
      }

      def transformActive(from: DenseMatrix[V], fn: (V) => V): Unit = {
        transform(from, fn)
      }
    }
  }

  implicit def canMapKeyValuePairs[V, R: ClassTag]: CanMapKeyValuePairs[DenseMatrix[V], (Int, Int), V, R, DenseMatrix[R]] = {
    new CanMapKeyValuePairs[DenseMatrix[V], (Int, Int), V, R, DenseMatrix[R]] {
      override def map(from: DenseMatrix[V], fn: (((Int, Int), V) => R)) = {
        val data = new Array[R](from.data.length)
        var off = 0
        cforRange (0 until from.cols) { j =>
          cforRange (0 until from.rows) { i =>
            data(off) = fn(i -> j, from(i, j))
            off += 1
          }
        }
        DenseMatrix.create(from.rows, from.cols, data, 0, from.rows)
      }

      override def mapActive(from: DenseMatrix[V], fn: (((Int, Int), V) => R)): DenseMatrix[R] =
        map(from, fn)
    }
  }

  implicit def DM_canMapValues[@specialized(Int, Float, Double) V, @specialized(Int, Float, Double) R: ClassTag]: CanMapValues[DenseMatrix[V], V, R, DenseMatrix[R]] = {
    new DenseCanMapValues[DenseMatrix[V], V, R, DenseMatrix[R]] {

      override def map(from: DenseMatrix[V], fn: (V => R)): DenseMatrix[R] = {
        if (from.isContiguous) {
          mapContiguous(from, fn)
        } else {
          mapGeneral(from, fn)
        }
      }

      private def mapGeneral(from: DenseMatrix[V], fn: V => R) = {
        // TODO: convert to majorsize etc
        val data = new Array[R](from.size)
        var j = 0
        var off = 0
        while (j < from.cols) {
          var i = 0
          while (i < from.rows) {
            data(off) = fn(from(i, j))
            off += 1
            i += 1
          }
          j += 1
        }
        DenseMatrix.create[R](from.rows, from.cols, data, 0, from.rows)
      }

      private def mapContiguous(from: DenseMatrix[V], fn: V => R) = {
        val data = new Array[R](from.size)
        val isTranspose = from.isTranspose
        val off = from.offset
        val fd = from.data
        cforRange(0 until data.length) { i =>
          data(i) = fn(fd(i + off))
        }
        DenseMatrix.create(from.rows, from.cols, data, 0, if (isTranspose) from.cols else from.rows, isTranspose)
      }
    }
  }


  implicit def canCopyDenseMatrix[V: ClassTag]: CanCopy[DenseMatrix[V]] = new CanCopy[DenseMatrix[V]] {
    def apply(v1: DenseMatrix[V]) = {
      v1.copy
    }
  }

}