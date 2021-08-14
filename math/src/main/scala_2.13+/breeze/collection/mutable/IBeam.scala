package breeze.collection.mutable

import scala.collection.generic.Shrinkable
import scala.collection._

trait IBeam[T]
    extends Iterable[T]
    with mutable.Builder[T, IndexedSeq[T]]
    with mutable.Shrinkable[T]
    with mutable.Cloneable[IBeam[T]]
    with IterableOps[T, Iterable, IBeam[T]] {

  override def knownSize: Int = size

  protected def ordering: Ordering[T]

  /**
   * Returns information on whether or not it made it onto the beam, and also what got
   * evicted
   * @param x
   * @return
   */
  def checkedAdd(x: T): Beam.BeamResult[T]

  def addOne(x: T): this.type = {
    checkedAdd(x)
    this
  }

  protected override def fromSpecific(coll: IterableOnce[T]): IBeam[T] = ???

  protected override def newSpecificBuilder: scala.collection.mutable.Builder[T, IBeam[T]] = ???

  override def empty: IBeam[T] = ???
}
