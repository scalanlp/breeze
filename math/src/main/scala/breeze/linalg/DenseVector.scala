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

import scala.{specialized=>spec}
import breeze.generic._
import breeze.linalg.support._
import breeze.linalg.operators._
import breeze.math._
import breeze.util.{ArrayUtil, Isomorphism}
import breeze.storage.Zero
import scala.reflect.ClassTag
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import breeze.macros.expand
import scala.math.BigInt
import spire.syntax.cfor._
import CanTraverseValues.ValuesVisitor
import CanZipAndTraverseValues.PairValuesVisitor
import java.io.ObjectStreamException
import scalaxy.debug._

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
@SerialVersionUID(1L) // TODO: scala doesn't propagate this to specialized subclasses. Sigh.
class DenseVector[@spec(Double, Int, Float, Long) V](val data: Array[V],
                                               val offset: Int,
                                               val stride: Int,
                                               val length: Int) extends StorageVector[V]
                                              with VectorLike[V, DenseVector[V]] with Serializable{
  def this(data: Array[V]) = this(data, 0, 1, data.length)
  def this(data: Array[V], offset: Int) = this(data, offset, 1, data.length)
  def this(length: Int)(implicit man: ClassTag[V]) = this(new Array[V](length), 0, 1, length)



  // ensure that operators are all loaded.
  DenseVector.init()

  def repr: DenseVector[V] = this

  def activeSize = length

  def apply(i: Int): V = {
    if(i < - size || i >= size) throw new IndexOutOfBoundsException(i + " not in [-"+size+","+size+")")
    val trueI = if(i<0) i+size else i
    if (noOffsetOrStride) {
      data(trueI)
    } else {
      data(offset + trueI * stride)
    }
  }

  def update(i: Int, v: V): Unit = {
    if(i < - size || i >= size) throw new IndexOutOfBoundsException(i + " not in [-"+size+","+size+")")
    val trueI = if(i<0) i+size else i
    if (noOffsetOrStride) {
      data(trueI) = v
    } else {
      data(offset + trueI * stride) = v
    }
  }

  private[linalg] val noOffsetOrStride = offset == 0 && stride == 1
  @deprecated("This isn't actually any faster any more", "0.12-SNAPSHOT")
  def unsafeUpdate(i: Int, v: V): Unit = if (noOffsetOrStride) data(i) = v else data(offset+i*stride) = v

  private def checkIfSpecialized(): Unit = {
    if(data.isInstanceOf[Array[Double]] && getClass.getName() == "breeze.linalg.DenseVector") throw new Exception("...")
  }
  // uncomment to debug places where specialization fails
  //  checkIfSpecialized()


  def activeIterator: Iterator[(Int, V)] = iterator

  def activeValuesIterator: Iterator[V] = valuesIterator

  def activeKeysIterator: Iterator[Int] = keysIterator

  override def equals(p1: Any) = p1 match {
    case y: DenseVector[_] =>
      y.length == length && ArrayUtil.nonstupidEquals(data, offset, stride, length, y.data, y.offset, y.stride, y.length)
    case _ => super.equals(p1)
  }


  // TODO: this is only consistent if the hashcode of inactive elements is 0!!!
  override def hashCode(): Int = ArrayUtil.zeroSkippingHashCode(data, offset, stride, length)

  override def toString = {
    valuesIterator.mkString("DenseVector(",", ", ")")
  }

  /**
   * Returns a copy of this DenseVector. stride will always be 1, offset will always be 0.
   * @return
   */
  def copy: DenseVector[V] = {
    if (stride == 1) {
      val newData = ArrayUtil.copyOfRange(data, offset, offset + length)
      new DenseVector(newData)
    } else {
      implicit val man = ClassTag[V](data.getClass.getComponentType.asInstanceOf[Class[V]])
      val r = new DenseVector(new Array[V](length))
      r := this
      r
    }
  }

  /**
   * same as apply(i). Gives the value at the underlying offset.
   * @param i index into the data array
   * @return apply(i)
   */
  def valueAt(i: Int): V = apply(i)

  /**
    * Unsafe version of above, a way to skip the checks.
    */
  @deprecated("This isn't actually any faster any more", "0.12-SNAPSHOT")
  def unsafeValueAt(i: Int): V = data(offset + i * stride)

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
  override def foreach[@spec(Unit) U](fn: (V) => U): Unit = {
    if (stride == 1) { // ABCE stuff
      cforRange(offset until (offset + length)) { j =>
        fn(data(j))
      }
    } else {
      var i = offset
      cforRange(0 until length) { j =>
        fn(data(i))
        i += stride
      }
    }
  }

  /**
   * Slices the DenseVector, in the range [start,end] with a stride stride.
   * @param start
   * @param end
   * @param stride
   */
  def slice(start: Int, end: Int, stride: Int=1): DenseVector[V] = {
    if(start > end || start < 0) throw new IllegalArgumentException("Slice arguments " + start +", " +end +" invalid.")
    if(end > length || end < 0) throw new IllegalArgumentException("End " + end + "is out of bounds for slice of DenseVector of length " + length)
    new DenseVector(data, start * this.stride + offset, stride * this.stride, (end-start)/stride)
  }

  // <editor-fold defaultstate="collapsed" desc=" Conversions (DenseMatrix, Array, Scala Vector) ">

  /** Creates a copy of this DenseVector that is represented as a 1 by length DenseMatrix */
  def toDenseMatrix: DenseMatrix[V] = {
     copy.asDenseMatrix
  }

  /** Creates a view of this DenseVector that is represented as a 1 by length DenseMatrix */
  def asDenseMatrix: DenseMatrix[V] = {
    new DenseMatrix[V](1, length, data, offset, stride)
  }



  override def toArray(implicit cm: ClassTag[V]): Array[V] = if(stride == 1){
    ArrayUtil.copyOfRange(data, offset, offset + length)
  } else {
    val arr = new Array[V](length)
    var i = 0
    var off = offset
    while(i < length) {
      arr(i) = data(off)
      off += stride
      i += 1
    }
    arr
  }

  /**Returns copy of this [[breeze.linalg.DenseVector]] as a [[scala.Vector]]*/
  def toScalaVector()(implicit cm: ClassTag[V]): scala.Vector[V] = this.toArray.toVector
  // </editor-fold>

  @throws(classOf[ObjectStreamException])
  protected def writeReplace(): Object = {
    new DenseVector.SerializedForm(data, offset, stride, length)
  }

}



object DenseVector extends VectorConstructors[DenseVector]
                      with DenseVector_GenericOps
                      with DenseVectorOps
                      with DenseVector_OrderingOps
                      with DenseVector_SpecialOps {


  def zeros[@spec(Double, Int, Float, Long) V: ClassTag : Zero](size: Int): DenseVector[V] = {
    val data = new Array[V](size)
    if(size != 0 && data(0) != implicitly[Zero[V]].zero)
      ArrayUtil.fill(data, 0, data.length, implicitly[Zero[V]].zero)
    apply(data)
  }

  def apply[@spec(Double, Int, Float, Long) V](values: Array[V]): DenseVector[V] = {
    // ensure we get specialized implementations even from non-specialized calls
    (values:AnyRef) match {
      case v: Array[Double] => new DenseVector(v).asInstanceOf[DenseVector[V]]
      case v: Array[Float] => new DenseVector(v).asInstanceOf[DenseVector[V]]
      case v: Array[Int] => new DenseVector(v).asInstanceOf[DenseVector[V]]
      case v: Array[Long] => new DenseVector(v).asInstanceOf[DenseVector[V]]
      case _ => new DenseVector(values)
    }
  }

  /**
   *
   * Creates a new DenseVector using the provided array (not making a copy!). In generic contexts, prefer to
   * use this (or apply) instead of `new DenseVector[V](data, offset, stride, length)`, which in general
   * won't give specialized implementations.
   * @param rows
   * @param cols
   * @param data
   * @tparam V
   * @return
   */
  def create[V](data: Array[V], offset: Int, stride: Int, length: Int): DenseVector[V] = {
    (data:AnyRef) match {
      case v: Array[Double] => new DenseVector(v, offset = offset, stride = stride, length = length).asInstanceOf[DenseVector[V]]
      case v: Array[Float] => new DenseVector(v, offset = offset, stride = stride, length = length).asInstanceOf[DenseVector[V]]
      case v: Array[Int] => new DenseVector(v, offset = offset, stride = stride, length = length).asInstanceOf[DenseVector[V]]
      case v: Array[Long] => new DenseVector(v, offset = offset, stride = stride, length = length).asInstanceOf[DenseVector[V]]
      case _ => new DenseVector(data, offset = offset, stride = stride, length = length)
    }
  }

  def ones[@spec(Double, Int, Float, Long) V: ClassTag:Semiring](size: Int): DenseVector[V] = fill[V](size, implicitly[Semiring[V]].one)

  def fill[@spec(Double, Int, Float, Long) V: ClassTag:Semiring](size: Int, v: V): DenseVector[V] = {
    val r = apply(new Array[V](size))
    assert(r.stride == 1)
    ArrayUtil.fill(r.data, r.offset, r.length, v)
    r
  }

    // concatenation
  /**
   * Horizontal concatenation of two or more vectors into one matrix.
   * @throws IllegalArgumentException if vectors have different sizes
   */
  def horzcat[V: ClassTag:Zero](vectors: DenseVector[V]*): DenseMatrix[V] = {
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
  def vertcat[V](vectors: DenseVector[V]*)(implicit canSet: OpSet.InPlaceImpl2[DenseVector[V], DenseVector[V]], vman: ClassTag[V], zero: Zero[V]): DenseVector[V] = {
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

  implicit def canCreateZerosLike[V:ClassTag:Zero]:CanCreateZerosLike[DenseVector[V], DenseVector[V]] =
  new CanCreateZerosLike[DenseVector[V], DenseVector[V]] {
    def apply(v1: DenseVector[V]): DenseVector[V] = {
      zeros[V](v1.length)
    }
  }

  implicit def canCopyDenseVector[V:ClassTag]: CanCopy[DenseVector[V]] = {
    new CanCopy[DenseVector[V]] {
      def apply(v1: DenseVector[V]): DenseVector[V] = {
        v1.copy
      }
    }
  }



  implicit def negFromScale[V](implicit scale: OpMulScalar.Impl2[DenseVector[V], V, DenseVector[V]], field: Ring[V]) = {
    new OpNeg.Impl[DenseVector[V], DenseVector[V]] {
      override def apply(a : DenseVector[V]): DenseVector[V] = {
        scale(a, field.negate(field.one))
      }
    }
  }


  implicit def canMapValues[@specialized(Int, Float, Double) V, @specialized(Int, Float, Double) V2](implicit man: ClassTag[V2]): CanMapValues[DenseVector[V], V, V2, DenseVector[V2]] = {
    new CanMapValues[DenseVector[V], V, V2, DenseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def apply(from: DenseVector[V], fn: (V) => V2): DenseVector[V2] = {
        val out = new Array[V2](from.length)

        // threeway fork, following benchmarks and hotspot docs on Array Bounds Check Elimination (ABCE)
        // https://wikis.oracle.com/display/HotSpotInternals/RangeCheckElimination
        if (from.noOffsetOrStride) {
          fastestPath(out, fn, from.data)
        } else if (from.stride == 1) {
          mediumPath(out, fn, from.data, from.offset)
        } else {
          slowPath(out, fn, from.data, from.offset, from.stride)
        }
        DenseVector[V2](out)
      }

      private def mediumPath(out: Array[V2], fn: (V) => V2, data: Array[V], off: Int): Unit = {
        cforRange(0 until out.length) { j =>
          out(j) = fn(data(j + off))
        }
      }

      private def fastestPath(out: Array[V2], fn: (V) => V2, data: Array[V]): Unit = {
        cforRange(0 until out.length) { j =>
          out(j) = fn(data(j))
        }
      }

      final private def slowPath(out: Array[V2], fn: (V) => V2, data: Array[V], off: Int, stride: Int): Unit = {
        var i = 0
        var j = off
        while (i < out.length) {
          out(i) = fn(data(j))
          i += 1
          j += stride
        }
      }
    }
  }

  implicit def scalarOf[T]: ScalarOf[DenseVector[T], T] = ScalarOf.dummy

  implicit def canIterateValues[V]: CanTraverseValues[DenseVector[V], V] =

    new CanTraverseValues[DenseVector[V], V] {

      override def isTraversableAgain(from: DenseVector[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: DenseVector[V], fn: ValuesVisitor[V]): Unit = {
        fn.visitArray(from.data, from.offset, from.length, from.stride)
      }

    }


  implicit def canTraverseZipValues[V,W]: CanZipAndTraverseValues[DenseVector[V], DenseVector[W], V,W] =

    new CanZipAndTraverseValues[DenseVector[V], DenseVector[W], V,W] {
      /** Iterates all key-value pairs from the given collection. */
      def traverse(from1: DenseVector[V], from2: DenseVector[W], fn: PairValuesVisitor[V,W]): Unit = {
        if (from1.size != from2.size) {
          throw new IllegalArgumentException("Vectors to be zipped must have same size")
        }
        cfor(0)(i => i < from1.size, i => i+1)(i => {
          fn.visit(from1(i), from2(i))
        })
      }
  }


  implicit def canTraverseKeyValuePairs[V]: CanTraverseKeyValuePairs[DenseVector[V], Int, V] =

    new CanTraverseKeyValuePairs[DenseVector[V], Int, V] {
      def isTraversableAgain(from: DenseVector[V]): Boolean = true

      def traverse(from: DenseVector[V], fn: CanTraverseKeyValuePairs.KeyValuePairsVisitor[Int, V]): Unit = {
        import from._

        fn.visitArray((ind: Int)=> (ind - offset)/stride, data, offset, length, stride)
      }

    }


  implicit def canTransformValues[@specialized(Int, Float, Double) V]: CanTransformValues[DenseVector[V], V] =

    new CanTransformValues[DenseVector[V], V] {
      def transform(from: DenseVector[V], fn: (V) => V) {
        val data = from.data
        val length = from.length
        val stride = from.stride

        val offset = from.offset
        if (stride == 1)  {
          cforRange(offset until offset + length) { j =>
            data(j) = fn(data(j))
          }
        } else {
          slowPath(fn, data, length, stride, offset)
        }
      }

      private def slowPath(fn: (V) => V, data: Array[V], length: Int, stride: Int, offset: Int): Unit = {
        val end = offset + stride * length
        var j = offset
        while (j != end) {
          data(j) = fn(data(j))
          j += stride
        }
      }

      def transformActive(from: DenseVector[V], fn: (V) => V) {
        transform(from, fn)
      }
    }


  implicit def canMapPairs[V, V2](implicit man: ClassTag[V2]):CanMapKeyValuePairs[DenseVector[V], Int, V, V2, DenseVector[V2]] =

    new CanMapKeyValuePairs[DenseVector[V], Int, V, V2, DenseVector[V2]] {
      /**Maps all key-value pairs from the given collection. */
      def map(from: DenseVector[V], fn: (Int, V) => V2): DenseVector[V2] = {
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
        DenseVector[V2](arr)
      }

      /**Maps all active key-value pairs from the given collection. */
      def mapActive(from: DenseVector[V], fn: (Int, V) => V2): DenseVector[V2] = {
        map(from, fn)
      }
    }

  // slicing
  // specialize to get the good class
  implicit def canSlice[V]: CanSlice[DenseVector[V], Range, DenseVector[V]] = {
    new CanSlice[DenseVector[V], Range, DenseVector[V]] {
      def apply(v: DenseVector[V], re: Range): DenseVector[V] = {

        val range: Range = re.getRangeWithoutNegativeIndexes( v.length )

        require(range.isEmpty || range.last < v.length)
        require(range.isEmpty || range.start >= 0)
        DenseVector.create(v.data, offset = v.offset + v.stride * range.start, stride = v.stride * range.step, length = range.length)
      }
    }
  }

  implicit def canTransposeComplex: CanTranspose[DenseVector[Complex], DenseMatrix[Complex]] = {
    new CanTranspose[DenseVector[Complex], DenseMatrix[Complex]] {
      def apply(from: DenseVector[Complex]): DenseMatrix[Complex] = {
        new DenseMatrix(data = from.data map { _.conjugate },
                        offset = from.offset,
                        cols = from.length,
                        rows = 1,
                        majorStride = from.stride)
      }
    }
  }

  class CanZipMapValuesDenseVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV:ClassTag] extends CanZipMapValues[DenseVector[V],V,RV,DenseVector[RV]] {
    def create(length : Int) = DenseVector(new Array[RV](length))

    /**Maps all corresponding values from the two collection. */
    def map(from: DenseVector[V], from2: DenseVector[V], fn: (V, V) => RV): DenseVector[RV] = {
      require(from.length == from2.length, s"Vectors must have same length")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result.data(i) = fn(from(i), from2(i))
        i += 1
      }
      result
    }
  }


  implicit def zipMap[V, R:ClassTag]: CanZipMapValuesDenseVector[V, R] = new CanZipMapValuesDenseVector[V, R]
  implicit val zipMap_d: CanZipMapValuesDenseVector[Double, Double] = new CanZipMapValuesDenseVector[Double, Double]
  implicit val zipMap_f: CanZipMapValuesDenseVector[Float, Float] = new CanZipMapValuesDenseVector[Float, Float]
  implicit val zipMap_i: CanZipMapValuesDenseVector[Int, Int] = new CanZipMapValuesDenseVector[Int, Int]

  class CanZipMapKeyValuesDenseVector[@spec(Double, Int, Float, Long) V, @spec(Int, Double) RV:ClassTag] extends CanZipMapKeyValues[DenseVector[V],Int, V,RV,DenseVector[RV]] {
    def create(length : Int) = DenseVector(new Array[RV](length))

    /**Maps all corresponding values from the two collection. */
    def map(from: DenseVector[V], from2: DenseVector[V], fn: (Int, V, V) => RV): DenseVector[RV] = {
      require(from.length == from2.length, "Vector lengths must match!")
      val result = create(from.length)
      var i = 0
      while (i < from.length) {
        result.data(i) = fn(i, from(i), from2(i))
        i += 1
      }
      result
    }


    override def mapActive(from: DenseVector[V], from2: DenseVector[V], fn: ((Int), V, V) => RV): DenseVector[RV] = {
      map(from, from2, fn)
    }
  }


  implicit def zipMapKV[V, R:ClassTag]: CanZipMapKeyValuesDenseVector[V, R] = new CanZipMapKeyValuesDenseVector[V, R]

  implicit val canAddIntoD: OpAdd.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = {
    new OpAdd.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        canDaxpy(a, 1.0, b)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpAdd.type]].register(this)
    }
  }

  implicit object canDaxpy extends scaleAdd.InPlaceImpl3[DenseVector[Double], Double, DenseVector[Double]] with Serializable {
    def apply(y: DenseVector[Double], a: Double, x: DenseVector[Double]) {
      require(x.length == y.length, s"Vectors must have same length")
      // using blas here is always a bad idea.
      if (x.noOffsetOrStride && y.noOffsetOrStride) {
        val ad = x.data
        val bd = y.data
        cforRange(0 until x.length) { i =>
          bd(i) += ad(i) * a
        }
      } else {
        cforRange(0 until x.length) { i =>
          y(i) += x(i) * a
        }
      }
    }

  }
  implicitly[TernaryUpdateRegistry[Vector[Double], Double, Vector[Double], scaleAdd.type]].register(canDaxpy)

  implicit val canAddD: OpAdd.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    pureFromUpdate_Double(canAddIntoD)
  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpAdd.type, Vector[Double]]].register(canAddD)

  implicit val canSubIntoD: OpSub.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = {
    new OpSub.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        canDaxpy(a, -1.0, b)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSub.type]].register(this)
    }

  }
  implicit val canSubD: OpSub.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    pureFromUpdate_Double(canSubIntoD)
  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpSub.type, Vector[Double]]].register(canSubD)

  implicit object canDotD extends OpMulInner.Impl2[DenseVector[Double], DenseVector[Double], Double] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
      require(a.length == b.length, s"Vectors must have same length")
      if (a.noOffsetOrStride && b.noOffsetOrStride && a.length < DenseVectorSupportMethods.MAX_SMALL_DOT_PRODUCT_LENGTH) {
        DenseVectorSupportMethods.smallDotProduct_Double(a.data, b.data, a.length)
      } else {
        blasPath(a, b)
      }
    }

    val UNROLL_FACTOR = 6

    private def blasPath(a: DenseVector[Double], b: DenseVector[Double]): Double = {
      if ((a.length <= 300 || !usingNatives) && a.stride == 1 && b.stride == 1) {
        DenseVectorSupportMethods.dotProduct_Double(a.data, a.offset, b.data, b.offset, a.length)
      } else  {
        val boff = if (b.stride >= 0) b.offset else (b.offset + b.stride * (b.length - 1))
        val aoff = if (a.stride >= 0) a.offset else (a.offset + a.stride * (a.length - 1))
        blas.ddot(
          a.length, b.data, boff, b.stride, a.data, aoff, a.stride)
      }
    }

  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpMulInner.type, Double]].register(canDotD)


  /*
  TODO: scaladoc crashes on this. I don't know why. It makes me want to die a little.
  Returns the k-norm of this Vector.
  */
  @expand
  @expand.valify
  implicit def canNorm[@expand.args(Int, Float, Long, BigInt, Complex) T]: norm.Impl2[DenseVector[T], Double, Double] = {

    new norm.Impl2[DenseVector[T], Double, Double] {
      def apply(v: DenseVector[T], n: Double): Double = {
        import v._
        if (n == 1) {
          var sum = 0.0
          foreach (v => sum += v.abs.toDouble )
          sum
        } else if (n == 2) {
          var sum = 0.0
          foreach (v => { val nn = v.abs.toDouble; sum += nn * nn })
          math.sqrt(sum)
        } else if (n == Double.PositiveInfinity) {
          var max = 0.0
          foreach (v => { val nn = v.abs.toDouble; if (nn > max) max = nn })
          max
        } else {
          var sum = 0.0
          foreach (v => { val nn = v.abs.toDouble; sum += math.pow(nn,n) })
          math.pow(sum, 1.0 / n)
        }
      }
    }
  }

  /**
   *  Returns the p-norm of this Vector (specialized for Double).
   */
  implicit def canNorm_Double: norm.Impl2[DenseVector[Double], Double, Double] = {
    new norm.Impl2[DenseVector[Double], Double, Double] {
      def apply(v: DenseVector[Double], p: Double): Double = {
        if (p == 2) {
          var sq = 0.0
          v.foreach (x => sq += x * x)
          math.sqrt(sq)
        } else if (p == 1) {
          var sum = 0.0
          v.foreach (x => sum += math.abs(x))
          sum
        } else if (p == Double.PositiveInfinity) {
          var max = 0.0
          v.foreach (x => max = math.max(max, math.abs(x)))
          max
        } else if (p == 0) {
          var nnz = 0
          v.foreach (x => if (x != 0) nnz += 1)
          nnz
        } else {
          var sum = 0.0
          v.foreach (x => sum += math.pow(math.abs(x), p))
          math.pow(sum, 1.0 / p)
        }
      }
    }
  }

  implicit def canDim[E]: dim.Impl[DenseVector[E],Int] = new dim.Impl[DenseVector[E],Int] {
    def apply(v: DenseVector[E]): Int = v.length
  }

  // this produces bad spaces for builtins (inefficient because of bad implicit lookup)
  implicit def space[E](implicit field: Field[E], man: ClassTag[E]): MutableFiniteCoordinateField[DenseVector[E],Int,E] = {
    import field._
    implicit val cmv = canMapValues[E,E]
    MutableFiniteCoordinateField.make[DenseVector[E],Int,E]
  }

  implicit val space_Double: MutableFiniteCoordinateField[DenseVector[Double], Int, Double] = {
    MutableFiniteCoordinateField.make[DenseVector[Double],Int,Double]
  }

  implicit val space_Float: MutableFiniteCoordinateField[DenseVector[Float], Int, Float] = {
    MutableFiniteCoordinateField.make[DenseVector[Float],Int,Float]
  }

  implicit val space_Int: MutableFiniteCoordinateField[DenseVector[Int], Int, Int] = {
    MutableFiniteCoordinateField.make[DenseVector[Int],Int,Int]
  }

  implicit val space_Long: MutableFiniteCoordinateField[DenseVector[Long], Int, Long] = {
    MutableFiniteCoordinateField.make[DenseVector[Long],Int,Long]
  }

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


  /**
   * This class exists because @specialized instances don't respect the serial
   * @param data
   * @param offset
   * @param stride
   * @param length
   */
  @SerialVersionUID(1L)
  case class SerializedForm(data: Array[_],
                            offset: Int,
                            stride: Int,
                            length: Int) extends Serializable {

    @throws(classOf[ObjectStreamException])
    def readResolve():Object = {
      data match {//switch to make specialized happy
        case x: Array[Int] => new DenseVector(x, offset, stride, length)
        case x: Array[Long] => new DenseVector(x, offset, stride, length)
        case x: Array[Double] => new DenseVector(x, offset, stride, length)
        case x: Array[Float] => new DenseVector(x, offset, stride, length)
        case x: Array[Short] => new DenseVector(x, offset, stride, length)
        case x: Array[Byte] => new DenseVector(x, offset, stride, length)
        case x: Array[Char] => new DenseVector(x, offset, stride, length)
        case x: Array[_] => new DenseVector(x, offset, stride, length)
      }

    }
  }





  // used to make sure the operators are loaded
  @noinline
  private def init() = {}
}
