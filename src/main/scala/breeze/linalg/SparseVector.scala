package breeze.linalg
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import operators._
import scala.math.BigInt
import scala.{specialized=>spec}
import breeze.storage.{DefaultArrayValue}
import support.{CanZipMapValues, CanMapKeyValuePairs, CanCopy, CanSlice}
import breeze.util.{Sorting, ArrayUtil}
import breeze.generic._
import breeze.math.{Complex, Semiring, Ring, TensorSpace}
import breeze.collection.mutable.SparseArray
import java.util
import collection.mutable
import scala.reflect.ClassTag
import breeze.macros.expand
import breeze.generic.CanTraverseValues.ValuesVisitor


/**
 * A Binary-search backed vector.
 * There is a parallel array of ints (in 0 until length) and values, sorted by index value.
 * To quickly access all stored values use the following loop:
 *
 * {{{
 *  var offset = 0
 *  while( offset < v.activeSize) {
 *    val index: Int = v.indexAt(offset)
 *    val value: E = v.valueAt(offset)
 *
 *    offset += 1
 *  }
 * }}}
 *
 *@author dlwh
 */
@SerialVersionUID(1)
class SparseVector[@spec(Double,Int, Float) E](val array: SparseArray[E])
                                              (implicit value: DefaultArrayValue[E])
                                              extends StorageVector[E]
                                              with VectorLike[E, SparseVector[E]] with Serializable {

  /** This auxiliary constructor assumes that the index array is already sorted. */
  def this(index: Array[Int], data: Array[E], activeSize: Int, length: Int)(implicit value: DefaultArrayValue[E])  = this(new SparseArray(index, data, activeSize, length, value.value))
  /** This auxiliary constructor assumes that the index array is already sorted. */
  def this(index: Array[Int], data: Array[E], length: Int)(implicit value: DefaultArrayValue[E])  = this(index, data, index.length, length)

  def data  = array.data
  def index = array.index
  def activeSize = array.activeSize
  def used = activeSize
  def length = array.length

  def repr = this

  def contains(i: Int) = array.contains(i)

  def apply(i: Int) = {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    array(i)
  }

  def update(i: Int, v: E) {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    array(i) = v
  }

  def activeIterator = activeKeysIterator zip activeValuesIterator

  def activeValuesIterator = data.iterator.take(activeSize)

  def activeKeysIterator = index.iterator.take(activeSize)

  // TODO: allow this to vary
  /** This is always assumed to be equal to 0, for now. */
  def default = value.value

  override def equals(p1: Any) = p1 match {
    case x: Vector[_] =>
        this.length == x.length &&
          (valuesIterator sameElements x.valuesIterator)
    case _ => false
  }

  def isActive(rawIndex: Int) = array.isActive(rawIndex)

  override def toString = {
    activeIterator.mkString("SparseVector(",", ", ")")
  }

  def copy: SparseVector[E] = {
    new SparseVector[E](ArrayUtil.copyOf(index, index.length), ArrayUtil.copyOf(data, index.length), activeSize, size)
  }

  def reserve(nnz: Int) {
    array.reserve(nnz)
  }

  def compact() {
    array.compact()
  }

  /**
   * Sets the underlying sparse array to use this data
   * @param index must be a sorted list of indices
   * @param data values corresponding to the index
   * @param activeSize number of active elements. The first activeSize will be used.
   */
  def use(index: Array[Int], data: Array[E], activeSize: Int) {
    require(activeSize <= size, "Can't have more elements in the array than length!")
    require(activeSize >= 0, "activeSize must be non-negative")
    require(data.length >= activeSize, "activeSize must be no greater than array length...")
    array.use(index, data, activeSize)
  }

  /**
   * same as data(i). Gives the value at the underlying offset.
   * @param i index into the data array
   * @return
   */
  def valueAt(i: Int): E = data(i)

  /**
   * Gives the logical index from the physical index.
   * @param i
   * @return
   */
  def indexAt(i: Int): Int = index(i)

  /**
   * Only gives true if isActive would return true for all i. (May be false anyway)
   * @return
   */
  def allVisitableIndicesActive: Boolean = true

}

object SparseVector extends SparseVectorOps
            with DenseVector_SparseVector_Ops
            with SparseVector_DenseVector_Ops
            with UFunc2ZippingImplicits[SparseVector] {
  def zeros[@spec(Double, Float, Int) V: ClassTag:DefaultArrayValue](size: Int) = new SparseVector(Array.empty, Array.empty[V], 0, size)
  def apply[@spec(Double, Float, Int) V:DefaultArrayValue](values: Array[V]) = new SparseVector(Array.range(0,values.length), values, values.length, values.length)

  def apply[V:ClassTag:DefaultArrayValue](values: V*):SparseVector[V] = apply(values.toArray)
  def fill[@spec(Double, Int, Float) V:ClassTag:DefaultArrayValue](size: Int)(v: =>V):SparseVector[V] = apply(Array.fill(size)(v))
  def tabulate[@spec(Double, Int, Float) V:ClassTag:DefaultArrayValue](size: Int)(f: Int=>V):SparseVector[V]= apply(Array.tabulate(size)(f))

  def apply[V:ClassTag:DefaultArrayValue](length: Int)(values: (Int, V)*) = {
    val r = zeros[V](length)
    for( (i, v) <- values) {
      r(i) = v
    }
    r
  }

  def vertcat[V:DefaultArrayValue:ClassTag](vectors: SparseVector[V]*): SparseVector[V] = {
    val resultArray = vectors.map(_.array).foldLeft(new SparseArray[V](0))(_ concatenate _)
    new SparseVector(resultArray)
  }

  def horzcat[V:DefaultArrayValue:ClassTag](vectors: SparseVector[V]*):CSCMatrix[V] ={
    if(!vectors.forall(_.size==vectors(0).size))
      throw new IllegalArgumentException("vector lengths must be equal, but got: " + vectors.map(_.length).mkString(", "))
    val rows = vectors(0).length
    val cols = vectors.length
    val data = new Array[V](vectors.map(_.data.length).sum)
    val rowIndices = new Array[Int](data.length)
    val colPtrs = new Array[Int](vectors.length + 1)
    val used = data.length

    var vec = 0
    var off = 0
    while(vec < vectors.length) {
      colPtrs(vec) = off
      System.arraycopy(vectors(vec).data, 0, data, off, vectors(vec).activeSize)
      System.arraycopy(vectors(vec).index, 0, rowIndices, off, vectors(vec).activeSize)
      off += vectors(vec).activeSize
      vec += 1
    }
    colPtrs(vec) = off

    new CSCMatrix(data, rows, cols, colPtrs, used, rowIndices)
  }

  // implicits
  class CanCopySparseVector[@spec(Int, Float, Double) V:ClassTag:DefaultArrayValue] extends CanCopy[SparseVector[V]] {
    def apply(v1: SparseVector[V]) = {
      v1.copy
    }
  }

  implicit def canCopySparse[@spec(Int, Float, Double) V: ClassTag: DefaultArrayValue] = new CanCopySparseVector[V]

  implicit def canMapValues[V, V2: ClassTag: DefaultArrayValue]:CanMapValues[SparseVector[V], V, V2, SparseVector[V2]] = {
    new CanMapValues[SparseVector[V], V, V2, SparseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: SparseVector[V], fn: (V) => V2) = {
        SparseVector.tabulate(from.length)(i => fn(from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: SparseVector[V], fn: (V) => V2) = {
        val out = new Array[V2](from.activeSize)
        var i = 0
        while(i < from.activeSize) {
          out(i) = fn(from.data(i))
          i += 1
        }
        new SparseVector(from.index.take(from.activeSize), out, from.activeSize, from.length)
      }
    }
  }

  implicit def canIterateValues[V]:CanTraverseValues[SparseVector[V], V] = {
    new CanTraverseValues[SparseVector[V],V] {

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: SparseVector[V], fn: ValuesVisitor[V]): Unit = {
        fn.zeros(from.size - from.activeSize, from.default)
        var i = 0
        while(i < from.activeSize) {
          fn.visit(from.data(i))
          i += 1
        }
      }
    }
  }

  implicit def canTransformValues[V:DefaultArrayValue:ClassTag]:CanTransformValues[SparseVector[V], V, V] = {
    new CanTransformValues[SparseVector[V], V, V] {
      val z = implicitly[DefaultArrayValue[V]]
      /**Transforms all key-value pairs from the given collection. */
      def transform(from: SparseVector[V], fn: (V) => V) {
        val newData =  mutable.ArrayBuilder.make[V]()
        val newIndex = mutable.ArrayBuilder.make[Int]()
        var used = 0
        var i = 0
        while(i < from.length) {
          val vv = fn(from(i))
          if(vv != z) {
            newData += vv
            newIndex += i
            used += 1
          }
          i += 1
        }
        from.array.use(newIndex.result(), newData.result(), used)
      }

      /**Transforms all active key-value pairs from the given collection. */
      def transformActive(from: SparseVector[V], fn: (V) => V) {
        var i = 0
        while(i < from.activeSize) {
          from.data(i) = fn(from.data(i))
          i += 1
        }
      }
    }
  }


  implicit def canMapPairs[V, V2: ClassTag: DefaultArrayValue]:CanMapKeyValuePairs[SparseVector[V], Int, V, V2, SparseVector[V2]] = {
    new CanMapKeyValuePairs[SparseVector[V], Int, V, V2, SparseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: SparseVector[V], fn: (Int, V) => V2) = {
        SparseVector.tabulate(from.length)(i => fn(i, from(i)))
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: SparseVector[V], fn: (Int, V) => V2) = {
        val out = new Array[V2](from.used)
        var i = 0
        while(i < from.used) {
          out(i) = fn(from.index(i), from.data(i))
          i += 1
        }
        new SparseVector(from.index.take(from.used), out, from.used, from.length)
      }
    }
  }

  class CanZipMapValuesSparseVector[@spec(Int, Double, Float) V, @spec(Int, Double) RV:ClassTag:DefaultArrayValue] extends CanZipMapValues[SparseVector[V],V,RV,SparseVector[RV]] {
    def create(length : Int) = zeros(length)

    /**Maps all corresponding values from the two collection. */
    def map(from: SparseVector[V], from2: SparseVector[V], fn: (V, V) => RV) = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result(i) = fn(from(i), from2(i))
        i += 1
      }
      result
    }
  }
  implicit def zipMap[V, R:ClassTag:DefaultArrayValue] = new CanZipMapValuesSparseVector[V, R]
  implicit val zipMap_d = new CanZipMapValuesSparseVector[Double, Double]
  implicit val zipMap_f = new CanZipMapValuesSparseVector[Float, Float]
  implicit val zipMap_i = new CanZipMapValuesSparseVector[Int, Int]


  implicit def negFromScale[@spec(Int, Float, Double)  V, Double](implicit scale: BinaryOp[SparseVector[V], V, OpMulScalar, SparseVector[V]], field: Ring[V]) = {
    new UnaryOp[SparseVector[V], OpNeg, SparseVector[V]] {
      override def apply(a : SparseVector[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }


  implicit val space_d = TensorSpace.make[SparseVector[Double], Int, Double]
  implicit val space_f = TensorSpace.make[SparseVector[Float], Int, Float]
  implicit val space_i = TensorSpace.make[SparseVector[Int], Int, Int]
  
  implicit def canTranspose[V:ClassTag:DefaultArrayValue]: CanTranspose[SparseVector[V], CSCMatrix[V]] = {
    new CanTranspose[SparseVector[V], CSCMatrix[V]] {
      def apply(from: SparseVector[V]) = {
        val transposedMtx = CSCMatrix.zeros[V](1, from.length)
        var i = 0
        while (i < from.activeSize) {
          val c = from.index(i)
          transposedMtx(0, c) = from.data(i)
          i += 1
        }
        transposedMtx
      }
    }
  }
  
  implicit def canTransposeComplex: CanTranspose[SparseVector[Complex], CSCMatrix[Complex]] = {
    new CanTranspose[SparseVector[Complex], CSCMatrix[Complex]] {
      def apply(from: SparseVector[Complex]) = {
        val transposedMtx = CSCMatrix.zeros[Complex](1, from.length)
        var i = 0
        while (i < from.activeSize) {
          val c = from.index(i)
          transposedMtx(0, c) = from.data(i).conjugate
          i += 1
        }
        transposedMtx
      }
    }
  }

}


trait SparseVector_DenseVector_Ops extends DenseVector_SparseVector_Ops { this: SparseVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  @expand.valify
  implicit def sv_dv_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[SparseVector[T], DenseVector[T], Op] = new BinaryUpdateOp[SparseVector[T], DenseVector[T], Op] {
    def apply(a: SparseVector[T], b: DenseVector[T]):Unit = {
      require(a.length == b.length, "Vectors must have the same length")
      val result = new VectorBuilder[T](a.length, a.length)
      val bd = b.data
      val adefault = a.array.default
      var boff = b.offset
      val asize = a.activeSize
      val bstride = b.stride
      val ad = a.data
      val ai = a.index

      var i = 0
      var j = 0
      while(i < asize) {
        // do defaults until we get to the next aoffset
        val nextBoff = b.offset + ai(i) * bstride
        while(boff < nextBoff) {
          result.add(j, op(adefault, bd(boff)))
          boff += bstride
          j += 1
        }

        result.add(j, op(ad(i), bd(boff)))
        boff += b.stride
        i += 1
        j += 1
      }

      while(boff < bd.length) {
        result.add(j, op(adefault, bd(boff)))
        boff += bstride
        j += 1
      }

      val rs = result.toSparseVector(true, true)
      a.use(rs.index, rs.data, rs.activeSize)
    }
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_dv_op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
                        @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _}, {_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[SparseVector[T], DenseVector[T], Op, DenseVector[T]] = {
    new BinaryOp[SparseVector[T], DenseVector[T], Op, DenseVector[T]] {
      def apply(a: SparseVector[T], b: DenseVector[T]) = {
        require(a.length == b.length, "Vectors must have the same length")
        val result = DenseVector.zeros[T](a.length)
        val bd = b.data
        val adefault = a.array.default
        var boff = b.offset
        val asize = a.activeSize
        val bstride = b.stride
        val ad = a.data
        val ai = a.index

        var i = 0
        var j = 0
        while(i < asize) {
          // do defaults until we get to the next aoffset
          val nextBoff = b.offset + ai(i) * bstride
          while(boff < nextBoff) {
            result(j) = op(adefault, bd(boff))
            boff += bstride
            j += 1
          }

          result(j) = op(ad(i), bd(boff))
          boff += b.stride
          i += 1
          j += 1
        }

        while(boff < bd.length) {
          result(j) = op(adefault, bd(boff))
          boff += bstride
          j += 1
        }

        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op, Vector[T]]].register(this)
    }
  }

  @expand
  @expand.valify
  implicit def canDot_SV_DV[@expand.args(Int, Double, Float, Long, BigInt, Complex) T]: BinaryOp[SparseVector[T], DenseVector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[SparseVector[T], DenseVector[T], breeze.linalg.operators.OpMulInner, T] {
      def apply(a: SparseVector[T], b: DenseVector[T]) = {
        require(b.length == a.length, "Vectors must be the same length!")
        b dot a
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner, T]].register(this)
    }
  }


}


trait DenseVector_SparseVector_Ops { this: SparseVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def dv_sv_UpdateOp[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ * _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[DenseVector[T], SparseVector[T], Op] = new BinaryUpdateOp[DenseVector[T], SparseVector[T], Op] {
    def apply(a: DenseVector[T], b: SparseVector[T]):Unit = {
      require(a.length == b.length, "Vectors must have the same length")
      val ad = a.data
      val bdefault = b.array.default
      var aoff = a.offset
      val bsize = b.activeSize
      val astride = a.stride
      val bd = b.data
      val bi = b.index

      var i = 0
      while(i < bsize) {
        // do defaults until we get to the next aoffset
        val nextAoff = a.offset + bi(i) * astride
        while(aoff < nextAoff) {
          ad(aoff) = op(ad(aoff), bdefault)
          aoff += astride
        }

        ad(aoff) = op(ad(aoff), bd(i))
        aoff += a.stride
        i += 1
      }

      while(aoff < ad.length) {
        ad(aoff) = op(ad(aoff), bdefault)
        aoff += astride
      }
    }
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op]].register(this)
  }

  // this shouldn't be necessary but it is:
  @expand
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  @expand.valify
  implicit def dv_sv_op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType] = {
    //val _op: BinaryOp[DenseVector[T], SparseVector[T], Op, DenseVector[T]] =
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op, Vector[T]]].register {
      DenseVector.pureFromUpdate(implicitly[BinaryUpdateOp[DenseVector[T], SparseVector[T], Op]])
    }
  }


  @expand
  implicit def dv_sv_Update_Zero_Idempotent[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}) op: BinaryOp[T, T, Op, T]):BinaryUpdateOp[DenseVector[T], SparseVector[T], Op] = new BinaryUpdateOp[DenseVector[T], SparseVector[T], Op] {
    def apply(a: DenseVector[T], b: SparseVector[T]):Unit = {
      require(a.length == b.length, "Vectors must have the same length")
      val ad = a.data
      val bd = b.data
      val bi = b.index
      val bsize = b.iterableSize

      var i = 0
      while(i < bsize) {
        val aoff = a.offset + bi(i) * a.stride
        ad(aoff) = op(ad(aoff), bd(i))
        i += 1
      }
    }
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op]].register(this)
  }


  @expand
  @expand.valify
  implicit def canDot_DV_SV[@expand.args(Int, Double, Float, Long, BigInt, Complex) T](implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero) zero: T): BinaryOp[DenseVector[T], SparseVector[T], breeze.linalg.operators.OpMulInner, T] = {
    new BinaryOp[DenseVector[T], SparseVector[T], breeze.linalg.operators.OpMulInner, T] {
      def apply(a: DenseVector[T], b: SparseVector[T]) = {
        var result: T = zero

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        val adata = a.data
        val aoff = a.offset
        val stride = a.stride

        var i = 0
        if(stride == 1 && aoff == 0) {
          while(i < bsize) {
            result += adata(bi(i)) * bd(i)
            i += 1
          }
        } else {
          while(i < bsize) {
            result += adata(aoff + bi(i) * stride) * bd(i)
            i += 1
          }
        }
        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner, T]].register(this)
    }
  }


  @expand
  implicit def sv_dv_axpy[@expand.args(Int, Double, Float, Long, BigInt, Complex) T] (implicit  @expand.sequence[T](0, 0.0, 0f, 0l, BigInt(0), Complex.zero) zero: T):CanAxpy[T, SparseVector[T], DenseVector[T]] = new CanAxpy[T, SparseVector[T], DenseVector[T]] {
    def apply(a: T, x: SparseVector[T], y: DenseVector[T]) {
      require(x.length == y.length, "Vectors must be the same length!")
      val xsize = x.activeSize

      if(a == zero) return

      var xoff = 0
      while(xoff < xsize) {
        y(x.indexAt(xoff)) += a * x.valueAt(xoff)
        xoff += 1
      }

    }
  }

}

trait SparseVectorOps { this: SparseVector.type =>
  import breeze.math.PowImplicits._


  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_sv_Idempotent_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _}, {_ - _}) op: BinaryOp[T, T, Op, T],
  @expand.sequence[T](0, 0.0, 0f, 0l, BigInt(0), Complex.zero) zero: T):BinaryOp[SparseVector[T], SparseVector[T], Op, SparseVector[T]] = new BinaryOp[SparseVector[T], SparseVector[T], Op, SparseVector[T]] {
    def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val asize = a.activeSize
      val bsize = b.activeSize

      val q = zero

      val resultI = new Array[Int](asize + bsize)
      val resultV = new Array[T](asize + bsize)
      var resultOff = 0

      var aoff = 0
      var boff = 0


      // double loop:
      // b moves to catch up with a, then a takes a step (possibly bringing b along)
      while(aoff < asize) {

        while(boff < bsize && b.indexAt(boff) < a.indexAt(aoff)) {
          resultI(resultOff) = b.indexAt(boff)
          resultV(resultOff) = op(q, b.valueAt(boff))
          resultOff += 1
          boff += 1
        }

        val bvalue = if(boff < bsize && b.indexAt(boff) == a.indexAt(aoff)) {
          val bv = b.valueAt(boff)
          boff += 1
          bv
        }  else {
          q
        }
        resultI(resultOff) = a.indexAt(aoff)
        resultV(resultOff) = op(a.valueAt(aoff), bvalue)
        resultOff += 1
        aoff += 1
      }

      while(boff < bsize) {
        resultI(resultOff) = b.indexAt(boff)
        resultV(resultOff) = op(q, b.valueAt(boff))
        resultOff += 1
        boff += 1
      }

      if(resultOff != resultI.length) {
        new SparseVector[T](util.Arrays.copyOf(resultI, resultOff), util.Arrays.copyOf(resultV, resultOff), resultOff, a.length)
      } else {
        new SparseVector[T](resultI, resultV, resultOff, a.length)
      }
    }
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op, Vector[T]]].register(this)
  }


  @expand
  @expand.valify
  implicit def sv_sv_OpMul[@expand.args(Int, Double, Float, Long, BigInt, Complex) T](implicit
  @expand.sequence[T](0, 0.0, 0f,  0l, BigInt(0), Complex.zero) zero: T):BinaryOp[SparseVector[T], SparseVector[T], OpMulScalar, SparseVector[T]] = new BinaryOp[SparseVector[T], SparseVector[T], OpMulScalar, SparseVector[T]] {
    def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
      if(b.activeSize < a.activeSize)
        return apply(b, a)

      require(b.length == a.length, "Vectors must be the same length!")
      val asize = a.activeSize
      val bsize = b.activeSize

      val resultI = new Array[Int](math.min(asize, bsize))
      val resultV = new Array[T](math.min(asize, bsize))
      var resultOff = 0

      var aoff = 0
      var boff = 0
      // in principle we could do divide and conquer here
      // by picking the middle of a, figuring out where that is in b, and then recursing,
      // using it as a bracketing.

      // double loop:
      // b moves to catch up with a, then a takes a step (possibly bringing b along)
      while(aoff < asize) {
        val aind = a.indexAt(aoff)
        // the min reflects the invariant that index aind must be in the first aind active indices in b's index.
        boff = util.Arrays.binarySearch(b.index, boff, math.min(bsize, aind + 1), aind)
        if(boff < 0) {
          boff = ~boff
          if(boff == bsize) {
            // we're through the b array, so we're done.
            aoff = asize
          } else {
            // fast forward a until we get to the b we just got to
            val bind = b.indexAt(boff)
            var newAoff = util.Arrays.binarySearch(a.index, aoff, math.min(asize, bind + 1), bind)
            if(newAoff < 0) {
              newAoff = ~newAoff
              boff += 1
            }
            assert(newAoff > aoff, bind + " " + aoff + " " + newAoff + " " + a.index(aoff) + " " + a.index(newAoff) + " " + a + " " + b)
            aoff = newAoff
          }
        } else {
          // b is there, a is there, do the multiplication!
          resultI(resultOff) = aind
          resultV(resultOff) = a.valueAt(aoff) * b.valueAt(boff)
          aoff += 1
          boff += 1
          resultOff += 1
        }
      }

      if(resultOff != resultI.length) {
        new SparseVector[T](util.Arrays.copyOf(resultI, resultOff), util.Arrays.copyOf(resultV, resultOff), resultOff, a.length)
      } else {
        new SparseVector[T](resultI, resultV, resultOff, a.length)
      }
    }
    implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulScalar, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_sv_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[SparseVector[T], SparseVector[T], Op, SparseVector[T]] = new BinaryOp[SparseVector[T], SparseVector[T], Op, SparseVector[T]] {
    def apply(a: SparseVector[T], b: SparseVector[T]): SparseVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = new VectorBuilder[T](a.length)
      var i = 0
      while(i < a.length) {
        result.add(i, op(a(i), b(i)))
        i += 1
      }
      result.toSparseVector(true, true)
    }
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_v_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T]):BinaryOp[SparseVector[T], Vector[T], Op, SparseVector[T]] = new BinaryOp[SparseVector[T], Vector[T], Op, SparseVector[T]] {
    def apply(a: SparseVector[T], b: Vector[T]): SparseVector[T] = {
      require(b.length == a.length, "Vectors must be the same length!")
      val result = new VectorBuilder[T](a.length)
      var i = 0
      while(i < a.length) {
        result.add(i, op(a(i), b(i)))
        i += 1
      }
      result.toSparseVector(true, true)
    }
    implicitly[BinaryRegistry[Vector[T], Vector[T], Op, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_s_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpDiv, OpSet, OpMod, OpPow) Op <: OpType]
  (implicit @expand.sequence[Op]({_ + _},  {_ - _}, {_ / _}, {(a,b) => b}, {_ % _}, {_ pow _})
  op: BinaryOp[T, T, Op, T],
   @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero)
   zero: T):BinaryOp[SparseVector[T], T, Op, SparseVector[T]] = new BinaryOp[SparseVector[T], T, Op, SparseVector[T]] {
    def apply(a: SparseVector[T], b: T): SparseVector[T] = {
      val result = new VectorBuilder[T](a.length)

      var i = 0
      while(i < a.length) {
        val r =  op(a(i), b)
        if(r  != zero)
          result.add(i,r)
        i += 1
      }
      result.toSparseVector(true, true)
    }
    implicitly[BinaryRegistry[Vector[T], T, Op, Vector[T]]].register(this)
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_s_Op[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpMulScalar, OpMulMatrix) Op<:OpType]
  (implicit @expand.sequence[T](0, 0.0, 0.0f, 0l, BigInt(0), Complex.zero)
   zero: T):BinaryOp[SparseVector[T], T, Op, SparseVector[T]] = new BinaryOp[SparseVector[T], T, Op, SparseVector[T]] {
    def apply(a: SparseVector[T], b: T): SparseVector[T] = {
      val result = new VectorBuilder[T](a.length)

      var i = 0
      while(i < a.activeSize) {
        result.add(a.indexAt(i), a.valueAt(i) * b)
        i += 1
      }
      result.toSparseVector(true, true)
    }
    implicitly[BinaryRegistry[Vector[T], T, Op, Vector[T]]].register(this)
  }

  protected def updateFromPure[T, Op<:OpType, Other](implicit op: BinaryOp[SparseVector[T], Other, Op, SparseVector[T]]): BinaryUpdateOp[SparseVector[T], Other, Op] = {
    new BinaryUpdateOp[SparseVector[T], Other, Op] {
      def apply(a: SparseVector[T], b: Other) {
        val result = op(a, b)
        a.use(result.index, result.data, result.activeSize)
      }
    }
  }


  implicit def opSet[T]: BinaryUpdateOp[SparseVector[T], SparseVector[T], OpSet] = {
    new BinaryUpdateOp[SparseVector[T], SparseVector[T], OpSet] {
      def apply(a: SparseVector[T], b: SparseVector[T]) {
        val result = b.copy
        a.use(result.index, result.data, result.activeSize)
      }
    }
  }

  implicit def opSetS[T:Semiring:ClassTag]: BinaryUpdateOp[SparseVector[T], T, OpSet] = {
    val zero = implicitly[Semiring[T]].zero
    new BinaryUpdateOp[SparseVector[T], T, OpSet] {
      def apply(a: SparseVector[T], b: T) {
        if(b == zero) {
          a.use(new Array[Int](2), new Array[T](2), 0)
          return
        }
        val data = Array.fill(a.length)(b)
        val index = Array.range(0, a.length)
        a.use(index, data, a.length)
      }
    }
  }

  // this shouldn't be necessary but it is:
  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_sv_Update[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
  @expand.args(OpAdd, OpSub, OpDiv, OpPow, OpMod, OpMulScalar) Op <: OpType]: BinaryUpdateOp[SparseVector[T], SparseVector[T], Op] = {
    val uop = updateFromPure(implicitly[BinaryOp[SparseVector[T], SparseVector[T], Op, SparseVector[T]]])
    implicitly[BinaryUpdateRegistry[Vector[T], Vector[T], Op]].register(uop)
    uop
  }

  @expand
  @expand.valify
  @expand.exclude(Complex, OpMod)
  @expand.exclude(BigInt, OpPow)
  implicit def sv_s_Update[@expand.args(Int, Double, Float, Long, BigInt, Complex) T,
   @expand.args(OpAdd, OpSub, OpDiv, OpPow, OpMod, OpMulScalar, OpMulMatrix) Op <: OpType]: BinaryUpdateOp[SparseVector[T], T, Op]  = {
    val uop = updateFromPure(implicitly[BinaryOp[SparseVector[T], T, Op, SparseVector[T]]])
    implicitly[BinaryUpdateRegistry[Vector[T], T, Op]].register(uop)
    uop
  }

  @expand
  @expand.valify
  implicit def sv_sv_Dot
  [@expand.args(Int, Double, Float, Long, BigInt, Complex) T]
  (implicit @expand.sequence[T](0, 0.0, 0f,  0l, BigInt(0), Complex.zero) zero: T):BinaryOp[SparseVector[T], SparseVector[T], OpMulInner, T] = new BinaryOp[SparseVector[T], SparseVector[T], OpMulInner, T] {
    def apply(a: SparseVector[T], b: SparseVector[T]): T = {
      if(b.activeSize < a.activeSize)
        return apply(b, a)

      require(b.length == a.length, "Vectors must be the same length!")
      val asize = a.activeSize
      val bsize = b.activeSize

      var result:T = zero

      var aoff = 0
      var boff = 0
      // in principle we could do divide and conquer here
      // by picking the middle of a, figuring out where that is in b, and then recursing,
      // using it as a bracketing.

      // double loop:
      // b moves to catch up with a, then a takes a step (possibly bringing b along)
      while(aoff < asize) {
        val aind = a.indexAt(aoff)
        boff = util.Arrays.binarySearch(b.index, boff, math.min(bsize, aind + 1), aind)
        if(boff < 0) {
          boff = ~boff
          if(boff == bsize) {
            // we're through the b array, so we're done.
            aoff = asize
          } else {
            // fast forward a until we get to the b we just got to
            val bind = b.indexAt(boff)
            var newAoff = util.Arrays.binarySearch(a.index, aoff, math.min(asize, bind + 1), bind)
            if(newAoff < 0) {
              newAoff = ~newAoff
              boff += 1
            }
            assert(newAoff > aoff, aoff + " " + newAoff)
            aoff = newAoff
          }
        } else {
          // b is there, a is there, do the multiplication!
          result += a.valueAt(aoff) * b.valueAt(boff)
          aoff += 1
          boff += 1
        }
      }

      result
    }
    implicitly[BinaryRegistry[Vector[T], Vector[T], OpMulInner, T]].register(this)
  }



  @expand
  implicit def sv_sv_axpy[@expand.args(Int, Double, Float, Long, BigInt, Complex) T] (implicit  @expand.sequence[T](0, 0.0, 0f, 0l, BigInt(0), Complex.zero) zero: T):CanAxpy[T, SparseVector[T], SparseVector[T]] = new CanAxpy[T, SparseVector[T], SparseVector[T]] {
    def apply(a: T, x: SparseVector[T], y: SparseVector[T]) {
      require(x.length == y.length, "Vectors must be the same length!")
      val asize = y.activeSize
      val bsize = x.activeSize

      if(a == zero) return

      val resultI = new Array[Int](asize + bsize)
      val resultV = new Array[T](asize + bsize)
      var resultOff = 0

      var aoff = 0
      var boff = 0



      // double loop:
      // b moves to catch up with a, then a takes a step (possibly bringing b along)
      while(aoff < asize) {

        while(boff < bsize && x.indexAt(boff) < y.indexAt(aoff)) {
          resultI(resultOff) = x.indexAt(boff)
          resultV(resultOff) = a * x.valueAt(boff)
          resultOff += 1
          boff += 1
        }

        val bvalue = if(boff < bsize && x.indexAt(boff) == y.indexAt(aoff)) {
          val bv = a * x.valueAt(boff)
          boff += 1
          bv
        }  else {
          zero
        }
        resultI(resultOff) = y.indexAt(aoff)
        resultV(resultOff) = y.valueAt(aoff) + bvalue
        resultOff += 1
        aoff += 1
      }

      while(boff < bsize) {
        resultI(resultOff) = x.indexAt(boff)
        resultV(resultOff) = a * x.valueAt(boff)
        resultOff += 1
        boff += 1
      }

      if(resultOff != resultI.length) {
        y.use(util.Arrays.copyOf(resultI, resultOff), util.Arrays.copyOf(resultV, resultOff), resultOff)
      } else {
        y.use(resultI, resultV, resultOff)
      }
    }
  }




}