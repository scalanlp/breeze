package breeze.linalg

import breeze.linalg.support.CanSlice2

/**
 * TODO
 *
 * @author dlwh
 **/
trait Broadcasted[+T] extends NumericOps[Broadcasted[T]] {
  def underlying: T
}

trait BroadcastedLike[T, Self <: Broadcasted[T]] extends Broadcasted[T] with NumericOps[Self]


class Broadcaster

object * extends Broadcaster

object Broadcaster {
  implicit def canBroadcastSliceColumns[From, Slice1, To]
  (implicit cs2_:: : CanSlice2[From, Slice1, ::.type, To]): CanSlice2[From, Slice1, *.type, BroadcastedColumns[To]] = {
    new CanSlice2[From, Slice1, *.type, BroadcastedColumns[To]] {
      def apply(from: From, slice: Slice1, slice2: *.type): BroadcastedColumns[To] = {
        BroadcastedColumns(cs2_::(from, slice, ::))
      }
    }
  }


  implicit def canBroadcastColumns[From, Slice1]: CanSlice2[From, ::.type, *.type, BroadcastedColumns[From]] = {
    new CanSlice2[From, ::.type, *.type, BroadcastedColumns[From]] {
      def apply(from: From, slice: ::.type, slice2: *.type): BroadcastedColumns[From] = {
        BroadcastedColumns(from)
      }
    }
  }

  implicit def canBroadcastSliceRows[From, Slice1, To]
  (implicit cs2_:: : CanSlice2[From, ::.type, Slice1, To]): CanSlice2[From, *.type, Slice1, BroadcastedRows[To]] = {
    new CanSlice2[From, *.type, Slice1, BroadcastedRows[To]] {
      def apply(from: From, slice2: *.type, slice: Slice1): BroadcastedRows[To] = {
        BroadcastedRows(cs2_::(from, ::, slice))
      }
    }
  }


  implicit def canBroadcastRows[From, Slice1]: CanSlice2[From, *.type, ::.type, BroadcastedRows[From]] = {
    new CanSlice2[From, *.type, ::.type, BroadcastedRows[From]] {
      def apply(from: From,  slice2: *.type, slice: ::.type): BroadcastedRows[From] = {
        BroadcastedRows(from)
      }
    }
  }
}




