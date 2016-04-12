package breeze.linalg

import breeze.macros.expand
import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseKeyValuePairs
import breeze.linalg.support.CanTraverseKeyValuePairs.KeyValuePairsVisitor

/**
 * Returns the key that has maximum value
 */
object argmax extends UFunc {
  @expand
  implicit def reduce[T, I, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseKeyValuePairs[T, I, S], @expand.sequence[S](Int.MinValue, Double.NegativeInfinity, Float.NegativeInfinity, Long.MinValue) init: S): Impl[T, I] = new Impl[T, I] {
    def apply(v: T): I = {
      class SumVisitor extends KeyValuePairsVisitor[I, S] {
        var max = init
        var amax : I = _
        var visitedOne = false

        def visit(k: I, a: S): Unit = {
          if(a > max || !visitedOne) {
            max = a
            amax = k
          }
          visitedOne = true
        }


        def visitZeros(numZero: Int, zeroKeys: Iterator[I], zeroValue: S): Unit = {
          if(numZero != 0) {
            if(zeroValue > max || !visitedOne) {
              max = zeroValue
              amax = zeroKeys.next()
            }
            visitedOne = true
          }
        }


        override def visitArray(indices: Int=>I, arr: Array[S], offset: Int, length: Int, stride: Int): Unit = {
          var i = 0
          var off = offset
          while(i < length) {
            val a = arr(off)
            if(a > max || !visitedOne) {
              max = a
              amax = indices(off)
            }
            visitedOne = true
            i += 1
            off += stride
          }
        }
      }

      val visit = new SumVisitor

      iter.traverse(v, visit)
      if(!visit.visitedOne) throw new IllegalArgumentException(s"No values in $v!")

      visit.amax
    }

  }
}

object argmin extends UFunc {
  @expand
  implicit def reduce[T, I, @expand.args(Int, Double, Float, Long) S](implicit iter: CanTraverseKeyValuePairs[T, I, S], @expand.sequence[S](Int.MaxValue, Double.PositiveInfinity, Float.PositiveInfinity, Long.MaxValue) init: S): Impl[T, I] = new Impl[T, I] {
    def apply(v: T): I = {
      class SumVisitor extends KeyValuePairsVisitor[I, S] {
        var min = init
        var amin : I = _
        var visitedOne = false

        def visit(k: I, a: S): Unit = {
          visitedOne = true
          if(a <= min) {
            min = a
            amin = k
          }
        }

        def visitZeros(numZero: Int, zeroKeys: Iterator[I], zeroValue: S): Unit = {
          if(numZero != 0) {
            visitedOne = true
            if(zeroValue <= min) {
              min = zeroValue
              amin = zeroKeys.next()
            }
          }
        }

        override def visitArray(indices: Int=>I, arr: Array[S], offset: Int, length: Int, stride: Int): Unit = {
          var i = 0
          var off = offset
          while(i < length) {
            visitedOne = true
            val a = arr(off)
            if(a <= min) {
              min = a
              amin = indices(off)
            }
            i += 1
            off += stride
          }
        }
      }

      val visit = new SumVisitor

      iter.traverse(v, visit)
      if(!visit.visitedOne) throw new IllegalArgumentException(s"No values in $v!")

      visit.amin
    }

  }
}
