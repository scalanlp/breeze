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
import breeze.math.{Complex, TensorSpace, Semiring, Ring}
import breeze.util.{ArrayUtil, Isomorphism}
import breeze.storage.DefaultArrayValue
import scala.reflect.ClassTag
import com.github.fommil.netlib.BLAS.{getInstance => blas}
import breeze.macros.expand
import scala.math.BigInt
import CanTraverseValues.ValuesVisitor

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
    if(i < - size || i >= size) throw new IndexOutOfBoundsException(i + " not in [-"+size+","+size+")")
    val trueI = if(i<0) i+size else i
    data(offset + trueI * stride)
  }

  def update(i: Int, v: E) {
    if(i < - size || i >= size) throw new IndexOutOfBoundsException(i + " not in [-"+size+","+size+")")
    val trueI = if(i<0) i+size else i
    data(offset + trueI * stride) = v
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


  /**
   * Slices the DenseVector, in the range [start,end] with a stride stride.
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
                      with DenseVectorOps
                      with DenseVector_OrderingOps
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
  def vertcat[V](vectors: DenseVector[V]*)(implicit canSet: OpSet.InPlaceImpl2[DenseVector[V], DenseVector[V]], vman: ClassTag[V], dav: DefaultArrayValue[V]): DenseVector[V] = {
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

  def binaryOpFromUpdateOp[Op<:OpType, V, Other](implicit copy: CanCopy[DenseVector[V]], op: UFunc.InPlaceImpl2[Op, DenseVector[V], Other], man: ClassTag[V]):UFunc.UImpl2[Op, DenseVector[V], Other, DenseVector[V]] = {
    new UFunc.UImpl2[Op, DenseVector[V], Other, DenseVector[V]] {
      override def apply(a : DenseVector[V], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

  implicit def negFromScale[V](implicit scale: OpMulScalar.Impl2[DenseVector[V], V, DenseVector[V]], field: Ring[V]) = {
    new OpNeg.Impl[DenseVector[V], DenseVector[V]] {
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
  implicit def handholdCMV[T]= new CanMapValues.HandHold[DenseVector[T], T]

  implicit def canIterateValues[V]:CanTraverseValues[DenseVector[V], V] = {
    new CanTraverseValues[DenseVector[V], V] {


      def isTraversableAgain(from: DenseVector[V]): Boolean = true

      /** Iterates all key-value pairs from the given collection. */
      def traverse(from: DenseVector[V], fn: ValuesVisitor[V]): Unit = {
        fn.visitArray(from.data, from.offset, from.length, from.stride)
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
      def apply(v: DenseVector[Any], re: Range) = {

        val r = re.getRangeWithoutNegativeIndexes( v.length )

        require(r.isEmpty || r.last < v.length)
        require(r.isEmpty || r.start >= 0)
        new DenseVector(v.data, offset = v.offset + r.start, stride = v.stride * r.step, length = r.length)
      }
    }
  }


//  implicit def canSliceExtender[V]: CanSlice[DenseVector[V], RangeExtender, DenseVector[V]] = __canSliceExtender.asInstanceOf[CanSlice[DenseVector[V], RangeExtender, DenseVector[V]]]
//
//  private val __canSliceExtender = {
//    new CanSlice[DenseVector[Any], RangeExtender, DenseVector[Any]] {
//      def apply(v: DenseVector[Any], re: RangeExtender) = {
//        canSlice(v, re.getRange(v.length) )
//      }
//    }
//  }

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

  implicit val canAddIntoD: OpAdd.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = {
    new OpAdd.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        blas.daxpy(
          a.length, 1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpAdd.type]].register(this)
    }
  }

  implicit object canDaxpy extends CanAxpy[Double, DenseVector[Double], DenseVector[Double]] with Serializable {
    def apply(a: Double, x: DenseVector[Double], y: DenseVector[Double]) {
      require(x.length == y.length, "Vectors must have same length")
      blas.daxpy(
        x.length, a, x.data, x.offset, x.stride, y.data, y.offset, y.stride)
    }
  }

  implicit val canAddD: OpAdd.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    pureFromUpdate_Double(canAddIntoD)
  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpAdd.type, Vector[Double]]].register(canAddD)

  implicit val canSubIntoD: OpSub.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = {
    new OpSub.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        blas.daxpy(
          a.length, -1.0, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSub.type]].register(this)
    }

  }
  implicit val canSubD: OpSub.Impl2[DenseVector[Double], DenseVector[Double], DenseVector[Double]] = {
    pureFromUpdate_Double(canSubIntoD)
  }
  implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpSub.type, Vector[Double]]].register(canSubD)

  implicit val canDotD: OpMulInner.Impl2[DenseVector[Double], DenseVector[Double], Double] = {
    new OpMulInner.Impl2[DenseVector[Double], DenseVector[Double], Double] {
      def apply(a: DenseVector[Double], b: DenseVector[Double]) = {
        require(a.length == b.length, "Vectors must have same length")
        blas.ddot(
          a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
      }
      implicitly[BinaryRegistry[Vector[Double], Vector[Double], OpMulInner.type, Double]].register(this)
    }

  }

  implicit val canScaleIntoD: OpMulScalar.InPlaceImpl2[DenseVector[Double], Double] = {
    new OpMulScalar.InPlaceImpl2[DenseVector[Double], Double] {
      def apply(a: DenseVector[Double], b: Double) = {
        blas.dscal(
          a.length, b, a.data, a.offset, a.stride)
      }
      implicitly[BinaryUpdateRegistry[Vector[Double], Double, OpMulScalar.type]].register(this)
    }

  }
  implicit val canScaleD: OpMulScalar.Impl2[DenseVector[Double], Double, DenseVector[Double]] = {
    binaryOpFromUpdateOp(implicitly[CanCopy[DenseVector[Double]]], canScaleIntoD, implicitly[ClassTag[Double]])
  }
  implicitly[BinaryRegistry[Vector[Double], Double, OpMulScalar.type, Vector[Double]]].register(canScaleD)

  implicit val canSetD: OpSet.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] = new OpSet.InPlaceImpl2[DenseVector[Double], DenseVector[Double]] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) {
      blas.dcopy(
        a.length, b.data, b.offset, b.stride, a.data, a.offset, a.stride)
    }
    implicitly[BinaryUpdateRegistry[Vector[Double], Vector[Double], OpSet.type]].register(this)
  }


  /*
  TODO: scaladoc crashes on this. I don't know why. It makes me want to die a little.
  Returns the k-norm of this Vector.
  */
  @expand
  @expand.valify
  implicit def canNorm[@expand.args(Int, Double, Float, Long, BigInt, Complex) T]: norm.Impl2[DenseVector[T], Double, Double] = {

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

