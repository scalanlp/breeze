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
import scala.{specialized=>spec}
import breeze.generic._
import breeze.linalg.support._
import breeze.math.{Complex, TensorSpace, Semiring, Ring}
import breeze.util.{ArrayUtil, Isomorphism}
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import com.github.fommil.netlib.LAPACK.{getInstance => lapack}


/**
 * A DenseVector is the "obvious" implementation of a Vector, with one twist.
 * The underlying data may have more data than the Vector, represented using an offset
 * into the array (for the 0th element), and a stride that is how far elements are apart
 * from one another.
 *
 * The i'th element is at offset + i * stride
 *
 * @author dlwh
 *
 * @param data data array
 * @param offset index of the 0'th element
 * @param stride separation between elements
 * @param length number of elements
 */
@SerialVersionUID(1L)
class DenseVector[@spec(Double, Int, Float) E](val data: Array[E],
                                               val offset: Int,
                                               val stride: Int,
                                               val length: Int) extends StorageVector[E]
                                              with VectorLike[E, DenseVector[E]] with Serializable{
  def this(data: Array[E]) = this(data, 0, 1, data.length)
  def this(data: Array[E], offset: Int) = this(data, offset, 1, data.length)

  // uncomment to get all the ridiculous places where specialization fails.
 // if(data.isInstanceOf[Array[Double]] && getClass.getName() == "breeze.linalg.DenseVector") throw new Exception("...")

  def repr = this

  def activeSize = length

  def apply(i: Int) = {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    data(offset + i * stride)
  }

  def update(i: Int, v: E) {
    if(i < 0 || i > size) throw new IndexOutOfBoundsException(i + " not in [0,"+size+")")
    data(offset + i * stride) = v
  }

  def activeIterator = iterator

  def activeValuesIterator = valuesIterator

  def activeKeysIterator = keysIterator

  override def equals(p1: Any) = p1 match {
    case y: DenseVector[_] =>
      y.length == length && ArrayUtil.nonstupidEquals(data, offset, stride, length, y.data, y.offset, y.stride, y.length)
    case x: Vector[_] =>
//      length == x.length && (( stride == x.stride
//        && offset == x.offset
//        && data.length == x.data.length
//        && ArrayUtil.equals(data, x.data)
//      )  ||  (
          valuesIterator sameElements x.valuesIterator
//        ))

    case _ => false
  }

  override def toString = {
    valuesIterator.mkString("DenseVector(",", ", ")")
  }

  override def ureduce[Final](f: URFunc[E, Final]) = {
    if(offset == 0 && stride == 1) f(data, length)
    else f(data, offset, stride, length, {(_:Int) => true})
  }

  /**
   * Returns a copy of this DenseVector. stride will always be 1, offset will always be 0.
   * @return
   */
  def copy: DenseVector[E] = {
    implicit val man = ClassTag[E](data.getClass.getComponentType.asInstanceOf[Class[E]])
    val r = new DenseVector(new Array[E](length))
    r := this
    r
  }

  /**
   * same as apply(i). Gives the value at the underlying offset.
   * @param i index into the data array
   * @return apply(i)
   */
  def valueAt(i: Int): E = apply(i)

  /**
   * Gives the logical index from the physical index.
   * @param i
   * @return i
   */
  def indexAt(i: Int): Int = i

  /**
   * Always returns true.
   *
   * Some storages (namely HashStorage) won't have active
   * indices packed. This lets you know if the bin is
   * actively in use.
   * @param i index into index/data arrays
   * @return
   */
  def isActive(i: Int): Boolean = true


  /**
   * Always returns true.
   * @return
   */
  def allVisitableIndicesActive: Boolean = true

  /**
   * Faster foreach
   * @param fn
   * @tparam U
   */
  override def foreach[U](fn: (E) => U) {
    var i = offset
    var j = 0
    while(j < length) {
      fn(data(i))
      i += stride
      j += 1
    }
  }

  /** Creates a copy of this DenseVector that is represented as a 1 by length DenseMatrix */
  def toDenseMatrix: DenseMatrix[E] = {
     copy.asDenseMatrix
  }

  /** Creates a view of this DenseVector that is represented as a 1 by length DenseMatrix */
  def asDenseMatrix: DenseMatrix[E] = {
    new DenseMatrix[E](1, length, data, offset, stride)
  }

  /**Returns the k-norm of this Vector. */
  override def norm(n: Double)(implicit field: Ring[E]): Double = {
    if (n == 1) {
      var sum = 0.0
      foreach (v => sum += field.norm(v))
      sum
    } else if (n == 2) {
      var sum = 0.0
      foreach (v => { val nn = field.norm(v); sum += nn * nn })
      math.sqrt(sum)
    } else if (n == Double.PositiveInfinity) {
      var max = Double.NegativeInfinity
      foreach (v => { val nn = field.norm(v); if (nn > max) max = nn })
      max
    } else {
      var sum = 0.0
      foreach (v => { val nn = field.norm(v); sum += math.pow(nn,n) })
      math.pow(sum, 1.0 / n)
    }
  }

  /**
   * Slices the DenseVector, in the range (start,end] with a stride stride.
   * @param start
   * @param end
   * @param stride
   */
  def slice(start: Int, end: Int, stride: Int=1):DenseVector[E] = {
    if(start > end || start < 0) throw new IllegalArgumentException("Slice arguments " + start +", " +end +" invalid.")
    if(end > length || end < 0) throw new IllegalArgumentException("End " + end + "is out of bounds for slice of DenseVector of length " + length)
    new DenseVector(data, start + offset, stride * this.stride, (end-start)/stride)
  }

  override def toArray(implicit cm: ClassTag[E]) = if(stride == 1){
    ArrayUtil.copyOfRange(data, offset, offset + length)
  } else {
    val arr = new Array[E](length)
    var i = 0
    var off = offset
    while(i < length) {
      arr(i) = data(off)
      off += stride
      i += 1
    }
    arr
  }
}



object DenseVector extends VectorConstructors[DenseVector] with DenseVector_GenericOps
                      with DenseVectorOps_Int
                      with DenseVectorOps_Float
                      with DenseVectorOps_Double
                      with DenseVectorOps_Complex
                      with DenseVectorOps_SparseVector_Double
                      with DenseVectorOps_SparseVector_Float
                      with DenseVectorOps_SparseVector_Int
                      with DenseVectorOps_SparseVector_Complex
                      with DenseVectorOps_HashVector_Double
                      with DenseVectorOps_HashVector_Float
                      with DenseVectorOps_HashVector_Int
                      with DenseVectorOps_HashVector_Complex
                      with DenseVector_SpecialOps {
  def zeros[@spec(Double, Float, Int) V: ClassTag : DefaultArrayValue](size: Int) = {
    val data = new Array[V](size)
    if(size != 0 && data(0) != implicitly[DefaultArrayValue[V]].value)
      ArrayUtil.fill(data, 0, data.length, implicitly[DefaultArrayValue[V]].value)
    new DenseVector(data)
  }


  def apply[@spec(Double, Float, Int) V](values: Array[V]) = new DenseVector(values)
  def ones[@spec(Double, Float, Int) V: ClassTag:Semiring](size: Int) = {
    val r = apply(new Array[V](size))
    assert(r.stride == 1)
    ArrayUtil.fill(r.data, r.offset, r.length, implicitly[Semiring[V]].one)
    r
  }


    // concatenation
  /**
   * Horizontal concatenation of two or more vectors into one matrix.
   * @throws IllegalArgumentException if vectors have different sizes
   */
  def horzcat[V: ClassTag:DefaultArrayValue](vectors: DenseVector[V]*): DenseMatrix[V] = {
    val size = vectors.head.size
    if (!(vectors forall (_.size == size)))
      throw new IllegalArgumentException("All vectors must have the same size!")
    val result = DenseMatrix.zeros[V](size, vectors.size)
    for ((v, col) <- vectors.zipWithIndex)
      result(::, col) := v
    result
  }

  /**
   * Vertical concatenation of two or more column vectors into one large vector.
   */
  def vertcat[V](vectors: DenseVector[V]*)(implicit canSet: BinaryUpdateOp[DenseVector[V], DenseVector[V], OpSet], vman: ClassTag[V], dav: DefaultArrayValue[V]): DenseVector[V] = {
    val size = vectors.foldLeft(0)(_ + _.size)
    val result = zeros[V](size)
    var offset = 0
    for (v <- vectors) {
      result.slice(offset, offset + v.size) := v
      offset += v.size
    }
    result
  }

  // capabilities

  implicit def canCreateZerosLike[V:ClassTag:DefaultArrayValue] = new CanCreateZerosLike[DenseVector[V], DenseVector[V]] {
    def apply(v1: DenseVector[V]) = {
      zeros[V](v1.length)
    }
  }

  implicit def canCopyDenseVector[V:ClassTag]:CanCopy[DenseVector[V]] = new CanCopy[DenseVector[V]] {
    def apply(v1: DenseVector[V]) = {
      v1.copy
    }
  }

  def binaryOpFromBinaryUpdateOp[V, Other, Op<:OpType](implicit copy: CanCopy[DenseVector[V]], op: BinaryUpdateOp[DenseVector[V], Other, Op], man: ClassTag[V]) = {
    new BinaryOp[DenseVector[V], Other, Op, DenseVector[V]] {
      override def apply(a : DenseVector[V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def negFromScale[V](implicit scale: BinaryOp[DenseVector[V], V, OpMulScalar, DenseVector[V]], field: Ring[V]) = {
    new UnaryOp[DenseVector[V], OpNeg, DenseVector[V]] {
      override def apply(a : DenseVector[V]) = {
        scale(a, field.negate(field.one))
      }
    }
  }

  implicit def canMapValues[V, V2](implicit man: ClassTag[V2]):CanMapValues[DenseVector[V], V, V2, DenseVector[V2]] = {
    new CanMapValues[DenseVector[V], V, V2, DenseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: DenseVector[V], fn: (V) => V2) = {
        // this is slow
        // DenseVector.tabulate(from.length)(i => fn(from(i)))
        val arr = new Array[V2](from.length)

        val d = from.data
        val stride = from.stride

        var i = 0
        var j = from.offset
        while(i < arr.length) {
          arr(i) = fn(d(j))
          i += 1
          j += stride
        }
        new DenseVector[V2](arr)
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: DenseVector[V], fn: (V) => V2) = {
        map(from, fn)
      }
    }
  }


  implicit def canTransformValues[V]:CanTransformValues[DenseVector[V], V, V] = {
    new CanTransformValues[DenseVector[V], V, V] {
      def transform(from: DenseVector[V], fn: (V) => V) {
        val d = from.data
        val stride = from.stride

        var i = 0
        var j = from.offset
        while(i < from.length) {
          from.data(j) = fn(d(j))
          i += 1
          j += stride
        }
      }

      def transformActive(from: DenseVector[V], fn: (V) => V) {
        transform(from, fn)
      }
    }
  }

  implicit def canMapPairs[V, V2](implicit man: ClassTag[V2]):CanMapKeyValuePairs[DenseVector[V], Int, V, V2, DenseVector[V2]] = {
    new CanMapKeyValuePairs[DenseVector[V], Int, V, V2, DenseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: DenseVector[V], fn: (Int, V) => V2) = {
        // slow: DenseVector.tabulate(from.length)(i => fn(i, from(i)))
        val arr = new Array[V2](from.length)

        val d = from.data
        val stride = from.stride

        var i = 0
        var j = from.offset
        while(i < arr.length) {
          arr(i) = fn(i, d(j))
          i += 1
          j += stride
        }
        new DenseVector[V2](arr)
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: DenseVector[V], fn: (Int, V) => V2) = {
        map(from, fn)
      }
    }
  }

  // slicing
  implicit def canSlice[V]: CanSlice[DenseVector[V], Range, DenseVector[V]] = __canSlice.asInstanceOf[CanSlice[DenseVector[V], Range, DenseVector[V]]]

  private val __canSlice = {
    new CanSlice[DenseVector[Any], Range, DenseVector[Any]] {
      def apply(v: DenseVector[Any], r: Range) = {
        require(r.isEmpty || r.last < v.length)
        require(r.isEmpty || r.start >= 0)
        new DenseVector(v.data, offset = v.offset + r.start, stride = v.stride * r.step, length = r.length)
      }
    }
  }

  implicit def canSliceSuffix[V]: CanSlice[DenseVector[V], RangeSuffix, DenseVector[V]] = __canSliceSuffix.asInstanceOf[CanSlice[DenseVector[V], RangeSuffix, DenseVector[V]]]

  private val __canSliceSuffix = {
    new CanSlice[DenseVector[Any], RangeSuffix, DenseVector[Any]] {
      def apply(v: DenseVector[Any], r: RangeSuffix) = {
        canSlice(v, r.start until v.length)
      }
    }
  }


  implicit def canTranspose[V]: CanTranspose[DenseVector[V], DenseMatrix[V]] = {
    new CanTranspose[DenseVector[V], DenseMatrix[V]] {
      def apply(from: DenseVector[V]) = {
        new DenseMatrix(data = from.data, offset = from.offset, cols = from.length, rows = 1, majorStride = from.stride)
      }
    }
  }
  
  implicit def canTransposeComplex: CanTranspose[DenseVector[Complex], DenseMatrix[Complex]] = {
    new CanTranspose[DenseVector[Complex], DenseMatrix[Complex]] {
      def apply(from: DenseVector[Complex]) = {
        new DenseMatrix(data = from.data map { _.conjugate }, 
                        offset = from.offset, 
                        cols = from.length, 
                        rows = 1, 
                        majorStride = from.stride)
      }
    }
  }

  // There's a bizarre error specializing float's here.
  class CanZipMapValuesDenseVector[@specialized(Int, Double, Float) V, @specialized(Int, Double) RV:ClassTag] extends CanZipMapValues[DenseVector[V],V,RV,DenseVector[RV]] {
    def create(length : Int) = new DenseVector(new Array[RV](length))

    /**Maps all corresponding values from the two collection. */
    def map(from: DenseVector[V], from2: DenseVector[V], fn: (V, V) => RV) = {
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


  implicit def zipMap[V, R:ClassTag] = new CanZipMapValuesDenseVector[V, R]
  implicit val zipMap_d = new CanZipMapValuesDenseVector[Double, Double]
  implicit val zipMap_f = new CanZipMapValuesDenseVector[Float, Float]
  implicit val zipMap_i = new CanZipMapValuesDenseVector[Int, Int]

  implicit val canAddIntoD: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpAdd] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        blas.daxpy(
          a.length, 1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canAddInto_V_V_Double.register(this)
    }
  }

  implicit val canDaxpy: CanAxpy[Double, DenseVector[Double], DenseVector[Double]] = {
    new CanAxpy[Double, DenseVector[Double], DenseVector[Double]] {
      def apply(a: Double, x: DenseVector[Double], y: DenseVector[Double]) {
        require(x.length == y.length, "Vectors must have same length")
        blas.daxpy(
          x.length, a, x.data, x.offset, x.stride, y.data, y.offset, y.stride)
      }
    }
  }

  implicit val canAddD: BinaryOp[DenseVector[Double], DenseVector[Double], OpAdd, DenseVector[Double]] = {
    pureFromUpdate_Double(canAddIntoD)
  }
  Vector.canAdd_V_V_Double.register(canAddD)

  implicit val canSubIntoD: BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] = {
    new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSub] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        blas.daxpy(
          a.length, -1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canSubInto_V_V_Double.register(this)
    }

  }
  implicit val canSubD: BinaryOp[DenseVector[Double], DenseVector[Double], OpSub, DenseVector[Double]] = {
    pureFromUpdate_Double(canSubIntoD)
  }
  Vector.canSub_V_V_Double.register(canSubD)

  implicit val canDotD: BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] = {
    new BinaryOp[DenseVector[Double], DenseVector[Double], OpMulInner, Double] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        blas.ddot(
          a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canDotProductV_Double.register(this)
    }

  }

  implicit val canScaleIntoD: BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Double], Double, OpMulScalar] {
      def apply(a: DenseVector[Double], b: Double) = {
        blas.dscal(
          a.length, b, a.data, a.offset, a.stride)
      }
      Vector.canMulScalarInto_V_S_Double.register(this)
    }

  }
  implicit val canScaleD: BinaryOp[DenseVector[Double], Double, OpMulScalar, DenseVector[Double]] {def apply(a: DenseVector[Double], b: Double): DenseVector[Double]} = binaryOpFromBinaryUpdateOp(implicitly[CanCopy[DenseVector[Double]]], canScaleIntoD, implicitly[ClassTag[Double]])
  Vector.canMulScalar_V_S_Double.register(canScaleD)

  implicit val canSetD:BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] = new BinaryUpdateOp[DenseVector[Double], DenseVector[Double], OpSet] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) {
      blas.dcopy(
        a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
    }
    Vector.canSetInto_V_V_Double.register(this)
  }


  implicit val space_d = TensorSpace.make[DenseVector[Double], Int, Double]
  implicit val space_f = TensorSpace.make[DenseVector[Float], Int, Float]
  implicit val space_i = TensorSpace.make[DenseVector[Int], Int, Int]

  object TupleIsomorphisms {
    implicit object doubleIsVector extends Isomorphism[Double,DenseVector[Double]] {
      def forward(t: Double) = DenseVector(t)
      def backward(t: DenseVector[Double]) = { assert(t.size == 1); t(0)}
    }

    implicit object pdoubleIsVector extends Isomorphism[(Double,Double),DenseVector[Double]] {
      def forward(t: (Double,Double)) = DenseVector(t._1,t._2)
      def backward(t: DenseVector[Double]) = { assert(t.size == 2); (t(0),t(1))}
    }
  }
}

trait DenseVector_GenericOps { this: DenseVector.type =>
  implicit def canSet_DV_Generic[V]: BinaryUpdateOp[DenseVector[V], V, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseVector[V], V, breeze.linalg.operators.OpSet] {
      def apply(a: DenseVector[V], b: V) {
        val ad = a.data
        if(a.stride == 1) {
          ArrayUtil.fill(ad, a.offset, a.length, b)
          return
        }

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = b
          aoff += a.stride
          i += 1
        }

      }
    }
  }

  implicit def canSet_DV_DV_Generic[V]: BinaryUpdateOp[DenseVector[V], DenseVector[V], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseVector[V], DenseVector[V], breeze.linalg.operators.OpSet] {
      def apply(a: DenseVector[V], b: DenseVector[V]) {
        require(b.length == a.length, "Vectors must be the same length!")
        if(a.stride == b.stride && a.stride == 1) {
          System.arraycopy(b.data, b.offset, a.data, a.offset, a.length)
          return
        }

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }

      }
    }
  }

  implicit def canGaxpy[V:Semiring]: CanAxpy[V, DenseVector[V], DenseVector[V]] = {
    new CanAxpy[V, DenseVector[V], DenseVector[V]] {
      val ring = implicitly[Semiring[V]]
      def apply(s: V, b: DenseVector[V], a: DenseVector[V]) {
      require(b.length == a.length, "Vectors must be the same length!")
      val ad = a.data
      val bd = b.data
      var aoff = a.offset
      var boff = b.offset

      var i = 0
      while(i < a.length) {
        ad(aoff) = ring.+(ad(aoff),ring.*(s, bd(boff)))
        aoff += a.stride
        boff += b.stride
        i += 1
      }
      }
    }
  }


}

trait DenseVector_SpecialOps extends DenseVectorOps_Double { this: DenseVector.type =>
  // hyperspecialized operators
  implicit val canDot_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMulInner, Int] = {
    new BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMulInner, Int] {
      def apply(a: DenseVector[Int], b: DenseVector[Int]) = {
        require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset
        var result = 0

        var i = 0
        while(i < a.length) {
          result += ad(aoff) * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        result

      }
      Vector.canDotProductV_Int.register(this)
    }
  }

  implicit val canDot_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMulInner, Float] = {
    new BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMulInner, Float] {
      def apply(a: DenseVector[Float], b: DenseVector[Float]) = {
        require(b.length == a.length, "Vectors must be the same length!")
        blas.sdot(
          a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      Vector.canDotProductV_Float.register(this)
    }
  }
}
