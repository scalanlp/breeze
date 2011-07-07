package scalanlp.classify;
/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/



import scalanlp.util.Index;
import scalanlp.data._;

import java.io.{DataOutput,DataInput}
import scalala.tensor._;
import mutable.Counter;
import scalala.operators._
import bundles.MutableInnerProductSpace
import scalala.generic.math.CanNorm
import scalala.generic.collection.{CanMapValues, CanCreateZerosLike, CanViewAsTensor1}
import scalanlp.serialization.DataSerialization.ReadWritable
import scalanlp.serialization.{SerializationFormat, DataSerialization}
;

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
    (implicit viewT2 : T2<:<MatrixOps[T2], viewTL: TL <:<NumericOps[TL],
     vv: CanViewAsTensor1[TL,L,Double],
     add : BinaryOp[TL,TL,OpAdd,TL],
     mulTensors : BinaryOp[T2,TF,OpMulMatrixBy,TL]) extends Classifier[L,TF] with Serializable {
  def scores(o: TF) = {
    val r:TL = featureWeights * o;
    vv(r + intercepts)
  }
}

class LFMatrix[L,TF](emptyTF: =>TF) extends MutableNumericOps[LFMatrix[L,TF]] with MatrixOps[LFMatrix[L,TF]] {
  def repr = this;
  private val map = collection.mutable.Map[L,TF]();

  def empty = new LFMatrix[L,TF](emptyTF);

  def apply(label: L) = {
    map.getOrElseUpdate(label,emptyTF);
  }

  def update(label: L, tf: TF) = {
    map(label) = tf;
  }

  override def toString = {
    map.mkString("Weight Matrix{\n  ","\n  ","\n}")
  }


}

object LFMatrix {
  implicit def lfMatrixTimesTF[L,TF]
  (implicit inner: BinaryOp[TF,TF,OpMulInner,Double], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],TF,OpMulMatrixBy,mutable.Counter[L,Double]]  = {
    new BinaryOp[LFMatrix[L,TF],TF,OpMulMatrixBy,mutable.Counter[L,Double]] {
      def opType = OpMulMatrixBy;

      def apply(v1: LFMatrix[L, TF], v2: TF) = {
        val r = Counter[L,Double]();
        for( (l,tf) <- v1.map) {
          r(l) = tf dot v2;
        }
        r
      }
    }
  }

  implicit def lfBinaryOp[L,TF,Op<:OpType]
  (implicit op: BinaryOp[TF,Double,Op,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],Double,Op,LFMatrix[L,TF]]  = {
    new BinaryOp[LFMatrix[L,TF],Double,Op,LFMatrix[L,TF]] {
      def opType = op.opType;

      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        val r = v1.empty;
        for( (l,tf) <- v1.map) {
          r(l) = op(tf,v2);
        }
        r
      }
    }
  }

  implicit def lfBinaryOpBackwards[L,TF,Op<:OpType]
  (implicit op: BinaryOp[Double,TF,Op,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[Double,LFMatrix[L,TF],Op,LFMatrix[L,TF]]  = {
    new BinaryOp[Double,LFMatrix[L,TF],Op,LFMatrix[L,TF]] {
      def opType = op.opType;

      def apply(v2: Double, v1: LFMatrix[L, TF]) = {
        val r = v1.empty;
        for( (l,tf) <- v1.map) {
          r(l) = op(v2,tf);
        }
        r
      }
    }
  }

  implicit def lfBinaryTFOp[L,TF,Op<:OpType]
  (implicit op: BinaryOp[TF,TF,Op,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],LFMatrix[L,TF],Op,LFMatrix[L,TF]]  = {
    new BinaryOp[LFMatrix[L,TF],LFMatrix[L,TF],Op,LFMatrix[L,TF]] {
      def opType = op.opType;

      def apply(v2: LFMatrix[L,TF], v1: LFMatrix[L, TF]) = {
        val r = v1.empty;
        val visited = scala.collection.mutable.Set[L]()
        for( (l,tf) <- v1.map) {
          visited += l;
          r(l) = op(v2(l),tf);
        }

        for( (l,tf) <- v2.map if !visited(l)) {
          r(l) = op(tf,v1(l));
        }

        r
      }
    }
  }

  implicit def lfInnerOp[L,TF]
  (implicit op: BinaryOp[TF,TF,OpMulInner,Double], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],LFMatrix[L,TF],OpMulInner,Double]  = {
    new BinaryOp[LFMatrix[L,TF],LFMatrix[L,TF],OpMulInner,Double] {
      def opType = op.opType;

      def apply(v2: LFMatrix[L,TF], v1: LFMatrix[L, TF]) = {
        val visited = scala.collection.mutable.Set[L]()
        var r = 0.0
        for( (l,tf) <- v1.map) {
          visited += l;
          r += op(v2(l),tf);
        }

        for( (l,tf) <- v2.map if !visited(l)) {
          r += op(tf,v1(l));
        }
        r
      }
    }
  }

  implicit def lfBinaryOp2[L,TF,Op]
  (implicit op: BinaryOp[TF,Double,OpMul,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],Double,OpMulMatrixBy,LFMatrix[L,TF]]  = {
    new BinaryOp[LFMatrix[L,TF],Double,OpMulMatrixBy,LFMatrix[L,TF]] {
      def opType = OpMulMatrixBy;

      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        val r = v1.empty;
        for( (l,tf) <- v1.map) {
          r(l) = tf * v2;
        }
        r
      }
    }
  }

  implicit def lfUpdateOp[L,TF,Op<:OpType]
  (implicit op: BinaryUpdateOp[TF,Double,Op], numeric: TF=>NumericOps[TF])
  : BinaryUpdateOp[LFMatrix[L,TF],Double,Op]  = {
    new BinaryUpdateOp[LFMatrix[L,TF],Double,Op] {
      def opType = op.opType;

      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        val r = v1.empty;
        for( (l,tf) <- v1.map) {
          op(tf,v2);
        }
        r
      }
    }
  }

  implicit def lfBinaryTFUpdateOp[L,TF,Op<:OpType]
  (implicit op: BinaryUpdateOp[TF,TF,Op], numeric: TF=>NumericOps[TF])
  : BinaryUpdateOp[LFMatrix[L,TF],LFMatrix[L,TF],Op]  = {
    new BinaryUpdateOp[LFMatrix[L,TF],LFMatrix[L,TF],Op] {
      def opType = op.opType;

      def apply(v2: LFMatrix[L,TF], v1: LFMatrix[L, TF]) {
        val visited = scala.collection.mutable.Set[L]()
        for( (l,tf) <- v1.map) {
          visited += l;
          op(v2(l),tf);
        }

        for( (l,tf) <- v2.map if !visited(l)) {
          op(tf,v1(l));
        }

      }
    }
  }

  implicit def lfNorm[L,TF](implicit op: CanNorm[TF]) : CanNorm[LFMatrix[L,TF]] = {
    new CanNorm[LFMatrix[L,TF]] {
      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        v1.map.valuesIterator.map(op.apply(_,v2)).sum;
      }
    }
  }

  implicit def hasValuesMonadic[L,TF](lfMatrix: LFMatrix[L,TF])(implicit hasValues: TF=>HasValuesMonadic[TF,Double])
    : HasValuesMonadic[LFMatrix[L, TF], Double]  = {
    new HasValuesMonadic[LFMatrix[L,TF],Double] {
      def values = new ValuesMonadic[LFMatrix[L,TF],Double] {
        def repr = lfMatrix;
      }
    }
  }

  implicit def canMapValues[L,TF](implicit cmf: CanMapValues[TF,Double,Double,TF]) = new CanMapValues[LFMatrix[L,TF],Double,Double,LFMatrix[L,TF]] {
    def mapNonZero(from: LFMatrix[L, TF], fn: (Double) => Double) = {
      val r = from.empty;
      for( (l,tf) <- from.map) {
        r(l) = cmf.mapNonZero(tf,fn);
      }
      r
    }

    def map(from: LFMatrix[L, TF], fn: (Double) => Double) = {
      val r = from.empty;
      for( (l,tf) <- from.map) {
        r(l) = cmf.map(tf,fn);
      }
      r
    }
  }

}

object LinearClassifier {
  implicit def linearClassifierReadWritable[L,TF,T2,TL](implicit viewT2 : T2<:<MatrixOps[T2], viewTL: TL <:<NumericOps[TL],
                                                     vv: CanViewAsTensor1[TL,L,Double],
                                                     add : BinaryOp[TL,TL,OpAdd,TL],
                                                     mulTensors : BinaryOp[T2,TF,OpMulMatrixBy,TL],
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

/*
  def fromRegression[F](data: Iterable[Example[Boolean,Map[F,Double]]]) = {
    val featureIndex = Index[F]();
    val idata = ( for(e <- data.iterator) yield { 
        for( map <- e;
             fv <- map) yield {
        (featureIndex(fv._1),fv._2);
      }
    } ).toSeq

    val vdata = new DenseMatrix(data.size,featureIndex.size+1);
    val y = new DenseVector(data.size);
    for( (e,i) <- idata.iterator.zipWithIndex ) {
      y(i) = if(e.label) 1 else -1; 
      vdata(i,featureIndex.size) = 1;
      for( (f,v) <- e.features) {
        vdata(i,f) = v;
      }
    }

    val lI = DenseMatrix(featureIndex.size+1,featureIndex.size+1)();
    lI := diag(featureIndex.size+1)
    lI *= 1E-6;

    val xtx = lI;
    xtx :+= vdata.transpose * vdata;

    val xty = vdata.transpose * y value;
    val denseXtY = DenseVector(xty.size)(0);
    denseXtY := xty;
    val beta = xtx \ denseXtY;
    val betaArray = beta.valuesIterator.toSeq.toArray;
    val trueWeights = betaArray.take(betaArray.size-1).toArray;
    val falseWeights = Array.fill(trueWeights.size)(0.0);
    new LinearClassifier(featureIndex,List(true,false),Array(trueWeights,falseWeights),Array(betaArray.last,0));
  }

  def testLR = {
    import scalanlp.stats.distributions.Rand;
    
    val trueDataGen = for { 
      x <- Rand.gaussian(4,1);
      y <- Rand.gaussian(-3,1)
    } yield {
      Example(true, Map(1->x,2->y));
    }

    val falseDataGen = for { 
      x <- Rand.gaussian(-4,1);
      y <- Rand.gaussian(2,1)
    } yield {
      Example(false, Map(1->x,2->y));
    }

    val data = trueDataGen.sample(1000) ++ falseDataGen.sample(1000);

    LinearClassifier.fromRegression(data);
  }
  */
}
