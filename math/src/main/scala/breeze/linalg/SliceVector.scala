package breeze.linalg

import breeze.linalg.operators._
import breeze.linalg.support.CanTraverseKeyValuePairs.KeyValuePairsVisitor
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.linalg.support._
import breeze.macros.expand
import breeze.math.Semiring
import breeze.storage.Zero

import scala.reflect.ClassTag
import scala.{specialized => spec}

import spire.syntax.cfor._

/**
 * A SliceVector is a vector that is a view of another underlying tensor. For instance:
 * {{{
 * val m = DenseMatrix(...)
 * m( (1,2), (3,4), (4,5))
 * }}}
 *
 * will give a SliceVector such that apply/update at index 0 will map to m(1,2), index 1 to m(3,4), etc.
 *
 * @author dlwh
 */
class SliceVector[@spec(Int) K, @spec(Double, Int, Float, Long) V: ClassTag](
    val tensor: Tensor[K, V],
    val slices: IndexedSeq[K])
    extends Vector[V]
    with VectorLike[V, SliceVector[K, V]] {

  def apply(i: Int): V = tensor(slices(i))

  def update(i: Int, v: V): Unit = { tensor(slices(i)) = v }

  def copy: DenseVector[V] = DenseVector(slices.map(tensor.apply _): _*)

  def length: Int = slices.length

  def activeSize: Int = slices.length

  def repr: SliceVector[K, V] = this

  def activeKeysIterator: Iterator[Int] = keysIterator

  def activeIterator: Iterator[(Int, V)] = iterator

  def activeValuesIterator: Iterator[V] = valuesIterator

  override def toString = {
    valuesIterator.mkString("SliceVector(", ", ", ")")
  }
}

object SliceVector extends SliceVectorOps {
  implicit def scalarOf[K, T]: ScalarOf[SliceVector[K, T], T] = ScalarOf.dummy

  implicit def canMapKeyValuePairs[K, V, V2: ClassTag]
    : CanMapKeyValuePairs[SliceVector[K, V], Int, V, V2, DenseVector[V2]] = {
    new CanMapKeyValuePairs[SliceVector[K, V], Int, V, V2, DenseVector[V2]] {
      override def map(from: SliceVector[K, V], fn: (Int, V) => V2): DenseVector[V2] = {
        DenseVector.tabulate(from.length)(i => fn(i, from(i)))
      }

      override def mapActive(from: SliceVector[K, V], fn: (Int, V) => V2): DenseVector[V2] = {
        map(from, fn)
      }
    }
  }

  implicit def canMapValues[K, V, V2: ClassTag]: CanMapValues[SliceVector[K, V], V, V2, DenseVector[V2]] = {
    new CanMapValues[SliceVector[K, V], V, V2, DenseVector[V2]] {
      override def apply(from: SliceVector[K, V], fn: (V) => V2): DenseVector[V2] = {
        DenseVector.tabulate(from.length)(i => fn(from(i)))
      }

    }
  }

  implicit def canCreateZerosLike[K, V: ClassTag: Zero]: CanCreateZerosLike[SliceVector[K, V], DenseVector[V]] = {
    new CanCreateZerosLike[SliceVector[K, V], DenseVector[V]] {
      def apply(v1: SliceVector[K, V]): DenseVector[V] = {
        DenseVector.zeros[V](v1.length)
      }
    }
  }

  implicit def canIterateValues[K, V]: CanTraverseValues[SliceVector[K, V], V] =
    new CanTraverseValues[SliceVector[K, V], V] {

      def isTraversableAgain(from: SliceVector[K, V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: SliceVector[K, V], fn: ValuesVisitor[V]): Unit = {
        from.valuesIterator.foreach {
          fn.visit(_)
        }
      }

    }

  implicit def canIterateKeyValuePairs[K, V]: CanTraverseKeyValuePairs[SliceVector[K, V], Int, V] = {
    new CanTraverseKeyValuePairs[SliceVector[K, V], Int, V] {

      /** Traverses all values from the given collection. */
      override def traverse(from: SliceVector[K, V], fn: KeyValuePairsVisitor[Int, V]): Unit = {
        from.iterator.foreach {
          case (k, v) => fn.visit(k, v)
        }

      }

      def isTraversableAgain(from: SliceVector[K, V]): Boolean = true

    }
  }

  implicit def canTransformValues[K, V]: CanTransformValues[SliceVector[K, V], V] = {
    new CanTransformValues[SliceVector[K, V], V] {
      def transform(from: SliceVector[K, V], fn: (V) => V): Unit = {
        for (i <- 0 until from.length) {
          from(i) = fn(from(i))
        }
      }

      def transformActive(from: SliceVector[K, V], fn: (V) => V): Unit = {
        transform(from, fn)
      }
    }
  }
}

trait SliceVectorOps {
  import breeze.math.PowImplicits._

  @expand
  implicit def slv_v_Op[
      K,
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, { (a, b) =>
        b
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryRegistry[SliceVector[K, T], Vector[T], Op.type, DenseVector[T]] =
    new BinaryRegistry[SliceVector[K, T], Vector[T], Op.type, DenseVector[T]] {

      override protected def bindingMissing(a: SliceVector[K, T], b: Vector[T]): DenseVector[T] = {
        require(a.length == b.length)
        DenseVector.tabulate(a.length)(i => op(a(i), b(i)))
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }

  @expand
  implicit def slv_s_Op[
      K,
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, { (a, b) =>
        b
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryRegistry[SliceVector[K, T], T, Op.type, DenseVector[T]] =
    new BinaryRegistry[SliceVector[K, T], T, Op.type, DenseVector[T]] {

      override protected def bindingMissing(a: SliceVector[K, T], b: T): DenseVector[T] = {
        DenseVector.tabulate(a.length)(i => op(a(i), b))
      }
      implicitly[BinaryRegistry[Vector[T], T, Op.type, Vector[T]]].register(this)
    }

  @expand
  implicit def slv_v_InPlaceOp[
      K,
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, { (a, b) =>
        b
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[SliceVector[K, T], Vector[T], Op.type] =
    new BinaryUpdateRegistry[SliceVector[K, T], Vector[T], Op.type] {

      override protected def bindingMissing(a: SliceVector[K, T], b: Vector[T]): Unit = {
        cforRange(0 until a.length) { i =>
          a(i) = op(a(i), b(i))
        }
      }
      implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op.type]].register(this)
    }

  @expand
  implicit def slv_s_InPlaceOp[
      K,
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ * _ }, { _ / _ }, { (a, b) =>
        b
      }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryUpdateRegistry[SliceVector[K, T], T, Op.type] =
    new BinaryUpdateRegistry[SliceVector[K, T], T, Op.type] {

      override protected def bindingMissing(a: SliceVector[K, T], b: T): Unit = {
        cforRange(0 until a.length) { i =>
          a(i) = op(a(i), b)
        }
      }
      implicitly[BinaryUpdateRegistry[Vector[T], T, Op.type]].register(this)
    }

}
