package breeze.linalg.operators

import breeze.linalg.{DenseVector, HashVector, SparseVector, Vector}
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support._
import breeze.linalg.support.CanZipAndTraverseValues.PairValuesVisitor
import breeze.macros.cforRange
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
      var i = 0
      while (i < from.length) {
        result.data(i) = fn(from(i), from2(i))
        i += 1
      }
      result
    }
  }

  // TODO: probably should have just made this virtual and not ufunced
  implicit def canMapValues_V[V, V2: Zero](implicit man: ClassTag[V2]): CanMapValues[Vector[V], V, V2, Vector[V2]] = {
    new CanMapValues[Vector[V], V, V2, Vector[V2]] {
      def apply(from: Vector[V], fn: (V) => V2): Vector[V2] = from match {
        case sv: SparseVector[V] => sv.mapValues(fn)
        case hv: HashVector[V] => hv.mapValues(fn)
        case dv: DenseVector[V] => dv.mapValues(fn)
        case _ => DenseVector.tabulate(from.length)(i => fn(from(i)))
      }
    }
  }

  // TODO: probably should have just made this virtual and not ufunced
  implicit def canMapActiveValues_V[V, V2: Zero](implicit man: ClassTag[V2]): CanMapActiveValues[Vector[V], V, V2, Vector[V2]] = {
    new CanMapActiveValues[Vector[V], V, V2, Vector[V2]] {

      def apply(from: Vector[V], fn: (V) => V2): Vector[V2] = from match {
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

    def traverse(from: Vector[V], fn: ValuesVisitor[V]): Unit = {
      for (v <- from.valuesIterator) {
        fn.visit(v)
      }
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

      def traverse(from: DenseVector[V], fn: ValuesVisitor[V]): Unit = {
        fn.visitArray(from.data, from.offset, from.length, from.stride)
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
