package breeze.classify

import breeze.util.{Encoder, Index}
import breeze.linalg.{Counter, DenseVector, NumericOps}
import breeze.linalg.operators._
import breeze.linalg.support.{CanCreateZerosLike, CanCopy, CanZipMapValues, CanNorm}
import breeze.generic.{URFunc, UReduceable, CanMapValues}
import breeze.serialization.DataSerialization
import breeze.serialization.DataSerialization.ReadWritable
import breeze.math.{MutableCoordinateSpace, TensorSpace}

/**
 * This stupidly named class is a Label-Feature Matrix, which is to say that
 * it's a the weight matrix used by most of the classifier trainers. It's
 * basically a matrix with one row per label, and the rows are some Tensor type (TF).
 * TF is a mnemonic for Feature Tensor.
 *
 * @param data array of weights for each label
 * @param emptyTF an empty weight vector, used for creating zeros dense vectors
 * @param labelIndex label index
 * @tparam L label type
 * @tparam TF feature tensor type
 */
@SerialVersionUID(1L)
class LFMatrix[L,TF:ClassManifest](val data: Array[TF],
                              emptyTF: =>TF,
                              val labelIndex:Index[L]) extends NumericOps[LFMatrix[L,TF]] with Serializable {
  def this(emptyTF: => TF, labelIndex: Index[L]) = this(Array.fill(labelIndex.size)(emptyTF), emptyTF, labelIndex)

  def repr = this
  def numLabels = labelIndex.size

  def empty = new LFMatrix[L,TF](emptyTF, labelIndex)

  def apply(label: L) = {
    val i = labelIndex(label)
    data(i)
  }

  def apply(label: Int) = {
    data(label)
  }

  def update(label: L, tf: TF) = {
    data(labelIndex(label)) = tf
  }

  def update(label: Int, tf: TF) = {
    data(label) = tf
  }

  override def toString = {
    data.mkString("Weight Matrix{\n  ","\n  ","\n}")
  }

  def unindexed = new UnindexedLFMatrix(this)

}

object LFMatrix {
  implicit def lfMatrixTimesTF[L,TF]
  (implicit inner: BinaryOp[TF,TF,OpMulInner,Double], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],TF,OpMulMatrix,DenseVector[Double]]  = {
    new BinaryOp[LFMatrix[L,TF],TF,OpMulMatrix,DenseVector[Double]] {

      def apply(v1: LFMatrix[L, TF], v2: TF) = {
        val r = DenseVector.zeros[Double](v1.numLabels)
        for( i <- 0 until r.length) {
          r(i) = v1.data(i) dot v2
        }
        r
      }
    }
  }

  implicit def lfBinaryOp[L,TF,Op<:OpType]
  (implicit op: BinaryOp[TF,Double,Op,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],Double,Op,LFMatrix[L,TF]]  = {
    new BinaryOp[LFMatrix[L,TF],Double,Op,LFMatrix[L,TF]] {

      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        val r = v1.empty
        for( (tf,l) <- v1.data.zipWithIndex) {
          r(l) = op(tf,v2)
        }
        r
      }
    }
  }

  implicit def lfBinaryOpBackwards[L,TF,Op<:OpType]
  (implicit op: BinaryOp[Double,TF,Op,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[Double,LFMatrix[L,TF],Op,LFMatrix[L,TF]]  = {
    new BinaryOp[Double,LFMatrix[L,TF],Op,LFMatrix[L,TF]] {

      def apply(v2: Double, v1: LFMatrix[L, TF]) = {
        val r = v1.empty
        for( (tf, l) <- v1.data.zipWithIndex) {
          r(l) = op(v2,tf)
        }
        r
      }
    }
  }

  implicit def lfBinaryTFOp[L,TF,Op<:OpType]
  (implicit op: BinaryOp[TF,TF,Op,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],LFMatrix[L,TF],Op,LFMatrix[L,TF]]  = {
    new BinaryOp[LFMatrix[L,TF],LFMatrix[L,TF],Op,LFMatrix[L,TF]] {

      def apply(v2: LFMatrix[L,TF], v1: LFMatrix[L, TF]) = {
        val r = v1.empty
        require(v2.labelIndex == v1.labelIndex, "Indices must be the same!")
        for( (tf, l) <- v1.data.zipWithIndex) {
          r(l) = op(v2(l),tf)
        }

        r
      }
    }
  }

  implicit def lfInnerOp[L,TF]
  (implicit op: BinaryOp[TF,TF,OpMulInner,Double], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],LFMatrix[L,TF],OpMulInner,Double]  = {
    new BinaryOp[LFMatrix[L,TF],LFMatrix[L,TF],OpMulInner,Double] {
      def apply(v2: LFMatrix[L,TF], v1: LFMatrix[L, TF]) = {
        var r = 0.0
        for( (tf, l) <- v1.data.zipWithIndex) {
          r += op(v2(l),tf)
        }

        r
      }
    }
  }

  implicit def lfBinaryOp2[L,TF,Op]
  (implicit op: BinaryOp[TF,Double,OpMulScalar,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],Double,OpMulScalar,LFMatrix[L,TF]]  = {
    new BinaryOp[LFMatrix[L,TF],Double, OpMulScalar, LFMatrix[L,TF]] {

      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        val r = v1.empty
        for( (tf, l) <- v1.data.zipWithIndex) {
          r(l) = tf :* v2
        }
        r
      }
    }
  }

  implicit def lfUpdateOp[L,TF,Op<:OpType]
  (implicit op: BinaryUpdateOp[TF,Double,Op], numeric: TF=>NumericOps[TF])
  : BinaryUpdateOp[LFMatrix[L,TF],Double,Op]  = {
    new BinaryUpdateOp[LFMatrix[L,TF],Double,Op] {

      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        val r = v1.empty
        for( tf <- v1.data) {
          op(tf,v2)
        }
        r
      }
    }
  }

  implicit def lfBinaryTFUpdateOp[L,TF,Op<:OpType]
  (implicit op: BinaryUpdateOp[TF,TF,Op], numeric: TF=>NumericOps[TF])
  : BinaryUpdateOp[LFMatrix[L,TF],LFMatrix[L,TF],Op]  = {
    new BinaryUpdateOp[LFMatrix[L,TF],LFMatrix[L,TF],Op] {
      def apply(v2: LFMatrix[L,TF], v1: LFMatrix[L, TF]) {
        require(v2.labelIndex == v1.labelIndex)
        for( (tf, l) <- v1.data.zipWithIndex) {
          op(v2(l),tf)
        }

      }
    }
  }

  implicit def lfUnaryOp[L,TF,Op<:OpType]
  (implicit op: UnaryOp[TF,Op,TF], numeric: TF=>NumericOps[TF])
  : UnaryOp[LFMatrix[L,TF], Op, LFMatrix[L, TF]]  = {
    new UnaryOp[LFMatrix[L,TF],Op, LFMatrix[L, TF]] {
      def apply(v1: LFMatrix[L, TF]) = {
        val r = v1.empty
        for( (tf, l) <- v1.data.zipWithIndex) {
          r(l) = op(tf)
        }
        r
      }
    }
  }

  implicit def lfNorm[L,TF](implicit op: CanNorm[TF]) : CanNorm[LFMatrix[L,TF]] = {
    new CanNorm[LFMatrix[L,TF]] {
      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        math.pow(v1.data.iterator.map(op.apply(_,v2)).map(math.pow(_,v2)).sum, 1/v2)
      }
    }
  }

  implicit def canMapValues[L,TF, V](implicit cmf: CanMapValues[TF,V,V,TF]) = new CanMapValues[LFMatrix[L,TF],V,V,LFMatrix[L,TF]] {
    def mapActive(from: LFMatrix[L, TF], fn: (V) => V) = {
      val r = from.empty
      for( (tf, l) <- from.data.zipWithIndex) {
        r(l) = cmf.mapActive(tf,fn)
      }
      r
    }

    def map(from: LFMatrix[L, TF], fn: (V) => V) = {
      val r = from.empty
      for( (tf, l) <- from.data.zipWithIndex) {
        r(l) = cmf.map(tf,fn)
      }
      r
    }
  }


  implicit def canZipMapValues[L,TF, S](implicit cmf: CanZipMapValues[TF,S,S,TF]) = new CanZipMapValues[LFMatrix[L,TF],S,S,LFMatrix[L,TF]] {
    def map(from: LFMatrix[L, TF], other: LFMatrix[L, TF], fn: (S, S) => S) = {
      val r = from.empty
      for( (tf, l) <- from.data.zipWithIndex) {
        r(l) = cmf.map(tf, other(l), fn)
      }
      r
    }
  }

  implicit def canCopy[L,TF](implicit copy: CanCopy[TF]) = new CanCopy[LFMatrix[L,TF]] {
    def apply(from: LFMatrix[L, TF]) = {
      val r = from.empty
      for( (v, l) <- from.data.zipWithIndex) {
        r(l) = copy(v)
      }
      r
    }
  }

  implicit def canCreateZerosLike[L,TF](implicit zeros: CanCreateZerosLike[TF, TF]) = new CanCreateZerosLike[LFMatrix[L, TF], LFMatrix[L,TF]] {
    def apply(from: LFMatrix[L, TF]) = {
      val r = from.empty
      for( (v, l) <- from.data.zipWithIndex) {
        r(l) =zeros(v)
      }
      r
    }
  }

  implicit def lfReadWritable[L,TF](implicit formatL: DataSerialization.ReadWritable[L],
                                    formatTF: DataSerialization.ReadWritable[TF],
                                    zeros: CanCreateZerosLike[TF,TF], man: Manifest[TF]) = {
     new ReadWritable[LFMatrix[L,TF]] {
       def write(sink: DataSerialization.Output, what: LFMatrix[L,TF]) = {
         DataSerialization.write(sink, what.labelIndex)
         DataSerialization.write(sink, what.data)(DataSerialization.arrayReadWritable[TF])
       }

       def read(source: DataSerialization.Input) = {
         val index = DataSerialization.read[Index[L]](source)
         val map = DataSerialization.read[Array[TF]](source)(DataSerialization.arrayReadWritable[TF])
         def default = zeros(map.head)
         val ret = new LFMatrix[L,TF](map, default, index)
         ret
       }
     }
   }

  implicit def ureduceable[L, TF, I, V](implicit space: TensorSpace[TF, I, V]) = new UReduceable[LFMatrix[L, TF], V] {
    import space._
    def apply[Final](c: LFMatrix[L, TF], f: URFunc[V, Final]): Final = f(c.data.iterator.flatMap(_.valuesIterator))
  }

  implicit def coordSpace[L, V, I](implicit space: MutableCoordinateSpace[V, Double]) = {
    import space._
    MutableCoordinateSpace.make[LFMatrix[L, V], Double]
  }


}

/**
 * This is the unindexed weights matrix: it acts as a tensor over the label types, rather than
 * their indexed components
 */
@SerialVersionUID(1L)
class UnindexedLFMatrix[L,TF](val indexed: LFMatrix[L, TF])  extends NumericOps[UnindexedLFMatrix[L,TF]] with Serializable {
  def repr: UnindexedLFMatrix[L, TF] = this

  def labelIndex = indexed.labelIndex
}

object UnindexedLFMatrix {
  implicit def lfMatrixTimesTF[L,TF]
  (implicit inner: BinaryOp[TF,TF,OpMulInner,Double], numeric: TF=>NumericOps[TF])
  : BinaryOp[UnindexedLFMatrix[L,TF],TF,OpMulMatrix,Counter[L, Double]]  = {
    new BinaryOp[UnindexedLFMatrix[L,TF],TF,OpMulMatrix,Counter[L, Double]] {

      def apply(v1: UnindexedLFMatrix[L, TF], v2: TF) = {
        val dv = v1.indexed * v2
        Encoder.fromIndex(v1.labelIndex).decode(dv)
      }
    }
  }
}
