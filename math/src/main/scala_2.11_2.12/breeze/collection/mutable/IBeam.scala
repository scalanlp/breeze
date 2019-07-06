package breeze.collection.mutable

import scala.collection.generic.Shrinkable
import scala.collection.{IndexedSeq, Iterable, IterableLike, mutable}

trait IBeam[T]
    extends Iterable[T]
    with IterableLike[T, IBeam[T]]
    with mutable.Builder[T, IndexedSeq[T]]
    with Shrinkable[T]
    with mutable.Cloneable[IBeam[T]] {
  override protected[this] def newBuilder: mutable.Builder[T, IBeam[T]] =
    throw new NotImplementedError("This should have been overridden")
  def freshEmpty: IBeam[T] = newBuilder.result()

  /**
   * Returns information on whether or not it made it onto the beam, and also what got
   * evicted
   * @param x
   * @return
   */
  def checkedAdd(x: T): Beam.BeamResult[T]

  def +=(x: T): this.type = {
    checkedAdd(x)
    this
  }
}
