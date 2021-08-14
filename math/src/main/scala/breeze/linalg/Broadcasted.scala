package breeze.linalg

import breeze.linalg.support._

/**
 * A Broadcasted is a type that represents "broadcasting" (a la numpy).
 *
 * Unlike Numpy, broadcasting in Breeze is explicit:
 *   matrix(*, ::) lifts UFuncs and operators so that they apply over each row
 *   matrix(::, *) is the same, but for columns
 *
 * @author dlwh
 **/
trait Broadcasted[+T, B] extends NumericOps[Broadcasted[T, B]] {
  def underlying: T
}

trait BroadcastedLike[T, B, Self <: Broadcasted[T, B]] extends Broadcasted[T, B] with NumericOps[Self] {
  def map[U, Res](f: B => U)(implicit cmv: CanMapValues[Self, B, U, Res]): Res = {
    cmv.map(repr, f)
  }

  def foreach[U](f: B => U)(implicit cmv: CanForeachValues[Self, B]): Unit = {
    cmv.foreach(repr, f)
  }
}

class Broadcaster

object * extends Broadcaster

object Broadcaster {
  implicit def canBroadcastSliceColumns[From, Slice1, To, Col](
      implicit cs2_:: : CanSlice2[From, Slice1, ::.type, To],
      handhold: CanCollapseAxis.HandHold[From, Axis._0.type, Col])
    : CanSlice2[From, Slice1, *.type, BroadcastedColumns[To, Col]] = {
    new CanSlice2[From, Slice1, *.type, BroadcastedColumns[To, Col]] {
      def apply(from: From, slice: Slice1, slice2: *.type): BroadcastedColumns[To, Col] = {
        BroadcastedColumns(cs2_::(from, slice, ::))
      }
    }
  }

  implicit def canBroadcastColumns[From, Slice1, Col](
      implicit handhold: CanCollapseAxis.HandHold[From, Axis._0.type, Col])
    : CanSlice2[From, ::.type, *.type, BroadcastedColumns[From, Col]] = {
    new CanSlice2[From, ::.type, *.type, BroadcastedColumns[From, Col]] {
      def apply(from: From, slice: ::.type, slice2: *.type): BroadcastedColumns[From, Col] = {
        BroadcastedColumns(from)
      }
    }
  }

  implicit def canBroadcastSliceRows[From, Slice1, To, Row](
      implicit cs2_:: : CanSlice2[From, ::.type, Slice1, To],
      handhold: CanCollapseAxis.HandHold[From, Axis._1.type, Row])
    : CanSlice2[From, *.type, Slice1, BroadcastedRows[To, Row]] = {
    new CanSlice2[From, *.type, Slice1, BroadcastedRows[To, Row]] {
      def apply(from: From, slice2: *.type, slice: Slice1): BroadcastedRows[To, Row] = {
        BroadcastedRows(cs2_::(from, ::, slice))
      }
    }
  }

  implicit def canBroadcastRows[From, Slice1, Row](implicit handhold: CanCollapseAxis.HandHold[From, Axis._1.type, Row])
    : CanSlice2[From, *.type, ::.type, BroadcastedRows[From, Row]] = {
    new CanSlice2[From, *.type, ::.type, BroadcastedRows[From, Row]] {
      def apply(from: From, slice2: *.type, slice: ::.type): BroadcastedRows[From, Row] = {
        BroadcastedRows(from)
      }
    }
  }
}
