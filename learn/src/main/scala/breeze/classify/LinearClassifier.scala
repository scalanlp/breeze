package breeze.classify
/*
 Copyright 2010 David Hall, Daniel Ramage

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



import breeze.serialization.DataSerialization.ReadWritable
import breeze.serialization.{SerializationFormat, DataSerialization}
import breeze.linalg.{Counter, NumericOps}
import breeze.linalg.operators._
import breeze.math.VectorSpace
import breeze.linalg.support.{CanCopy, CanZipMapValues, CanNorm, CanCreateZerosLike}
import breeze.generic.CanMapValues

/**
 * A LinearClassifier is a multi-class classifier with decision
 * function:
 * <code>
 * \hat y_i = \arg\max_y w_y^T x_i + b_y
 * </code>
 *
 * @author dlwh
 *
 */
@SerialVersionUID(1L)
class LinearClassifier[L,T2, TL, TF]
    (val featureWeights: T2, val intercepts: TL)
    (implicit viewT2 : T2<:<NumericOps[T2], vspace: VectorSpace[TL, Double],
     mulTensors : BinaryOp[T2,TF,OpMulMatrix,TL]) extends Classifier[L,TF] with Serializable {
  import vspace._
  def scores(o: TF) = {
    val r:TL = featureWeights * o
    r + intercepts
  }
}

object LinearClassifier {
  implicit def linearClassifierReadWritable[L,TF,T2,TL](implicit viewT2 : T2<:<NumericOps[T2], vspace: VectorSpace[TL, Double],
                                                        mulTensors : BinaryOp[T2,TF,OpMulMatrix,TL],
                                                        tfW: DataSerialization.ReadWritable[T2],
                                                        tlW: DataSerialization.ReadWritable[TL]) = {
    new ReadWritable[LinearClassifier[L,T2,TL,TF]] {
      def write(sink: DataSerialization.Output, what: LinearClassifier[L,T2,TL,TF]) = {
        tfW.write(sink,what.featureWeights)
        tlW.write(sink,what.intercepts)
      }

      def read(source: DataSerialization.Input) = {
        val t2 = tfW.read(source)
        val tl = tlW.read(source)
        new LinearClassifier(t2,tl)
      }
    }
  }
}

class LFMatrix[L,TF](emptyTF: =>TF) extends NumericOps[LFMatrix[L,TF]] with Serializable {
  def repr = this
  private val map = collection.mutable.Map[L,TF]()

  def empty = new LFMatrix[L,TF](emptyTF)

  def apply(label: L) = {
    map.getOrElseUpdate(label,emptyTF)
  }

  def update(label: L, tf: TF) = {
    map(label) = tf
  }

  override def toString = {
    map.mkString("Weight Matrix{\n  ","\n  ","\n}")
  }

}

object LFMatrix {
  implicit def lfMatrixTimesTF[L,TF]
  (implicit inner: BinaryOp[TF,TF,OpMulInner,Double], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],TF,OpMulMatrix,Counter[L,Double]]  = {
    new BinaryOp[LFMatrix[L,TF],TF,OpMulMatrix,Counter[L,Double]] {

      def apply(v1: LFMatrix[L, TF], v2: TF) = {
        val r = Counter[L,Double]()
        for( (l,tf) <- v1.map) {
          r(l) = tf dot v2
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
        for( (l,tf) <- v1.map) {
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
        for( (l,tf) <- v1.map) {
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
        val visited = scala.collection.mutable.Set[L]()
        for( (l,tf) <- v1.map) {
          visited += l
          r(l) = op(v2(l),tf)
        }

        for( (l,tf) <- v2.map if !visited(l)) {
          r(l) = op(tf,v1(l))
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
        val visited = scala.collection.mutable.Set[L]()
        var r = 0.0
        for( (l,tf) <- v1.map) {
          visited += l
          r += op(v2(l),tf)
        }

        for( (l,tf) <- v2.map if !visited(l)) {
          r += op(tf,v1(l))
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
        for( (l,tf) <- v1.map) {
          r(l) = tf * v2
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
        for( (l,tf) <- v1.map) {
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
        val visited = scala.collection.mutable.Set[L]()
        for( (l,tf) <- v1.map) {
          visited += l
          op(v2(l),tf)
        }

        for( (l,tf) <- v2.map if !visited(l)) {
          op(tf,v1(l))
        }

      }
    }
  }

  implicit def lfNorm[L,TF](implicit op: CanNorm[TF]) : CanNorm[LFMatrix[L,TF]] = {
    new CanNorm[LFMatrix[L,TF]] {
      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        v1.map.valuesIterator.map(op.apply(_,v2)).sum
      }
    }
  }

  implicit def canMapValues[L,TF](implicit cmf: CanMapValues[TF,Double,Double,TF]) = new CanMapValues[LFMatrix[L,TF],Double,Double,LFMatrix[L,TF]] {
    def mapActive(from: LFMatrix[L, TF], fn: (Double) => Double) = {
      val r = from.empty
      for( (l,tf) <- from.map) {
        r(l) = cmf.mapActive(tf,fn)
      }
      r
    }

    def map(from: LFMatrix[L, TF], fn: (Double) => Double) = {
      val r = from.empty
      for( (l,tf) <- from.map) {
        r(l) = cmf.map(tf,fn)
      }
      r
    }
  }


  implicit def canZipMapValues[L,TF](implicit cmf: CanZipMapValues[TF,Double,Double,TF]) = new CanZipMapValues[LFMatrix[L,TF],Double,Double,LFMatrix[L,TF]] {
    def map(from: LFMatrix[L, TF], other: LFMatrix[L, TF], fn: (Double, Double) => Double) = {
      val r = from.empty
      val keys = from.map.keySet ++ other.map.keySet
      for( l <- keys) {
        r(l) = cmf.map(from(l), other(l), fn)
      }
      r
    }
  }

  implicit def canCopy[L,TF](implicit copy: CanCopy[TF]) = new CanCopy[LFMatrix[L,TF]] {
    def apply(from: LFMatrix[L, TF]) = {
      val r = from.empty
      for( (l,v) <- from.map) {
        r(l) = copy(v)
      }
      r
    }
  }

  implicit def lfReadWritable[L,TF](implicit formatL: DataSerialization.ReadWritable[L], formatTF: DataSerialization.ReadWritable[TF], zeros: CanCreateZerosLike[TF,TF]) = {
     new ReadWritable[LFMatrix[L,TF]] {
       def write(sink: DataSerialization.Output, what: LFMatrix[L,TF]) = {
         DataSerialization.write(sink, what.map.toMap)
       }

       def read(source: DataSerialization.Input) = {
         val map = DataSerialization.read[Map[L,TF]](source)
         def default = zeros(map.head._2)
         val ret = new LFMatrix[L,TF](default)
         for( (k,v) <- map) {
           ret(k) = v
         }
         ret
       }
     }
   }


}


