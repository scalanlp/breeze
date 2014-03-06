package breeze.linalg

import breeze.generic.UFunc
import breeze.macros.expand
import breeze.linalg.support.{CanTransformValues, CanMapValues, CanTraverseValues}
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

//ToDo: minMax function to find both in one go

object max extends UFunc {
  @expand
  implicit def reduce[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S], @expand.sequence[S](Int.MinValue, Double.NegativeInfinity, Float.NegativeInfinity, Long.MinValue) init: S): Impl[T, S] = new Impl[T, S] {
    def apply(v: T): S = {
      class SumVisitor extends ValuesVisitor[S] {
        var max = init
        var visitedOne = false
        def visit(a: S): Unit = {
          visitedOne = true
          max = scala.math.max(max, a)
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
          if(numZero != 0) {
            visitedOne = true
            max = scala.math.max(zeroValue, max)
          }
        }

        override def visitArray(arr: Array[S], offset: Int, length: Int, stride: Int): Unit = {
          var i = 0
          var off = offset
          while(i < length) {
            visitedOne = true
            max = scala.math.max(max, arr(off))
            i += 1
            off += stride
          }
          max
        }
      }

      val visit = new SumVisitor

      iter.traverse(v, visit)
      if(!visit.visitedOne) throw new IllegalArgumentException(s"No values in $v!")

      visit.max
    }

  }

  /**
   * Method for computing the max of the first length elements of an array. Arrays
   * of size 0 give Double.NegativeInfinity
   * @param arr
   * @param length
   * @return
   */
  def array(arr: Array[Double], length: Int) = {
    var accum = Double.NegativeInfinity
    var i = 0
    while(i < length) {
      accum = scala.math.max(arr(i), accum)
      i += 1
    }
    accum
  }
}




/**
 * Computes the minimum.
 */
object min extends UFunc {
  @expand
  implicit def reduce[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S], @expand.sequence[S](Int.MaxValue, Double.PositiveInfinity, Float.PositiveInfinity, Long.MaxValue) init: S): Impl[T, S] = new Impl[T, S] {
    def apply(v: T): S = {
      class MinVisitor extends ValuesVisitor[S] {
        var min = init
        var visitedOne = false
        def visit(a: S): Unit = {
          visitedOne = true
          min = scala.math.min(min, a)
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
          if(numZero != 0) {
            visitedOne = true
            min = scala.math.min(zeroValue, min)
          }
        }

        override def visitArray(arr: Array[S], offset: Int, length: Int, stride: Int): Unit = {
          var i = 0
          var off = offset
          while(i < length) {
            visitedOne = true
            min = scala.math.min(min, arr(off))
            i += 1
            off += stride
          }
          min
        }
      }

      val visit = new MinVisitor

      iter.traverse(v, visit)
      if(!visit.visitedOne) throw new IllegalArgumentException(s"No values in $v!")

      visit.min
    }

  }
}

/**
 * clip(a, lower, upper) returns an array such that all elements are "clipped" at the range (lower, upper)
 */
object clip extends UFunc {
  implicit def clipOrdering[T,V](implicit ordering: Ordering[V], cmv: CanMapValues[T, V, V, T]):Impl3[T, V, V, T] = {
    new Impl3[T, V, V, T] {
      import ordering.mkOrderingOps
      def apply(v: T, v2: V, v3: V): T = {
        cmv.map(v, x => if(x < v2) v2 else if (x > v3) v3 else x)
      }
    }
  }

  implicit def clipInPlaceOrdering[T,V](implicit ordering: Ordering[V], cmv: CanTransformValues[T, V, V]):InPlaceImpl3[T, V, V] = {
    import ordering.mkOrderingOps
    new InPlaceImpl3[T, V, V] {
      def apply(v: T, v2: V, v3: V):Unit = {
        cmv.transform(v, x => if(x < v2) v2 else if (x > v3) v3 else x)
      }
    }
  }
}




object ptp extends UFunc {
  @expand
  implicit def reduce[T, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseValues[T, S], @expand.sequence[S](Int.MinValue, Double.NegativeInfinity, Float.NegativeInfinity, Long.MinValue) init: S): Impl[T, S] = new Impl[T, S] {
    def apply(v: T): S = {
      class SumVisitor extends ValuesVisitor[S] {
        var max = init
        var min = -init
        var visitedOne = false
        def visit(a: S): Unit = {
          visitedOne = true
          max = scala.math.max(max, a)
          min = scala.math.min(min, a)
        }

        def zeros(numZero: Int, zeroValue: S): Unit = {
          if(numZero != 0) {
            visitedOne = true
            max = scala.math.max(zeroValue, max)
            min = scala.math.min(zeroValue, min)
          }
        }

        override def visitArray(arr: Array[S], offset: Int, length: Int, stride: Int): Unit = {
          var i = 0
          var off = offset
          while(i < length) {
            visitedOne = true
            max = scala.math.max(max, arr(off))
            min = scala.math.min(min, arr(off))
            i += 1
            off += stride
          }
        }
      }

      val visit = new SumVisitor

      iter.traverse(v, visit)
      if(!visit.visitedOne) throw new IllegalArgumentException(s"No values in $v!")

      visit.max - visit.min
    }

  }


}