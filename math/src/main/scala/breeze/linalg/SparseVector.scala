package breeze.linalg

import scala.{specialized=>spec}
import breeze.storage.{DefaultArrayValue, SparseStorage}
import support.{CanCopy, CanSlice}
import breeze.util.ArrayUtil
import breeze.generic.{URFunc, UReduceable}


/**
 *
 * @author dlwh
 */
final class SparseVector[@spec(Double,Int,Float) E](var index: Array[Int],
                                                    var data: Array[E],
                                                    var used: Int,
                                                    val length: Int)(
                                                    implicit value: DefaultArrayValue[E]) extends Vector[E] with SparseStorage[E] with VectorLike[E, SparseVector[E]] {


  def repr = this

  def apply(i: Int) = {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    rawApply(i)
  }

  def update(i: Int, v: E) {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    rawUpdate(i, v)
  }

  def activeIterator = activeKeysIterator zip activeValuesIterator

  def activeValuesIterator = data.iterator.take(used)

  def activeKeysIterator = index.iterator.take(used)

  // TODO: allow this to vary
  // This is always assumed to be equal to 0, for now.
  def default = value.value

  override def equals(p1: Any) = p1 match {
    case x: SparseVector[_] =>
//      length == x.length && (( stride == x.stride
//        && offset == x.offset
//        && data.length == x.data.length
//        && ArrayUtil.equals(data, x.data)
//      )  ||  (
        this.length == x.length &&
        this.default == x.default &&
          (activeValuesIterator sameElements x.activeValuesIterator)
//        ))

    case _ => false
  }

  override def toString = {
    activeIterator.mkString("SparseVector(",", ", ")")
  }

  override def ureduce[Final](f: URFunc[E, Final]) = {
    f(data, used)
  }


}

object SparseVector extends SparseVectorOps_Int with SparseVectorOps_Float with SparseVectorOps_Double {
  def zeros[@spec(Double, Float, Int) V: ClassManifest:DefaultArrayValue](size: Int) = new SparseVector(Array.empty, Array.empty[V], 0, size)
  def apply[@spec(Double, Float, Int) V:DefaultArrayValue](values: Array[V]) = new SparseVector(Array.range(0,values.length), values, values.length, values.length)

  def apply[V:ClassManifest:DefaultArrayValue](values: V*):SparseVector[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float) V:ClassManifest:DefaultArrayValue](size: Int)(v: =>V):SparseVector[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassManifest:DefaultArrayValue](size: Int)(f: Int=>V):SparseVector[V]= apply(Array.tabulate(size)(f))


  class CanCopySparseVector[@specialized V:ClassManifest:DefaultArrayValue] extends CanCopy[SparseVector[V]] {
    def apply(v1: SparseVector[V]) = {
      new SparseVector[V](ArrayUtil.copyOf(v1.index, v1.index.length), ArrayUtil.copyOf(v1.data, v1.index.length), v1.used, v1.length)
    }
  }

  implicit def canCopySparse[V: ClassManifest: DefaultArrayValue] = new CanCopySparseVector[V]

}