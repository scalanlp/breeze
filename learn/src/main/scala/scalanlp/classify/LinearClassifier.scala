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

import scalala.tensor._;
import mutable.Counter;
import scalala.generic.collection.{CanCreateZerosLike, CanViewAsTensor1}
import scalala.operators._
import scalala.generic.math.CanNorm
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
@serializable
@SerialVersionUID(1L)
class LinearClassifier[L,F,T2, TL, TF]
    (val featureWeights: T2, val intercepts: TL)
    (implicit viewT2 : T2<:<MatrixOps[T2], viewTL: TL <:<NumericOps[TL],
     vv: CanViewAsTensor1[TL,L,Double],
     add : BinaryOp[TL,TL,OpAdd,TL],
     mulTensors : BinaryOp[T2,TF,OpMulMatrixBy,TL]) extends Classifier[L,TF] {
  def scores(o: TF) = {
    val r:TL = featureWeights * o;
    vv(r + intercepts)
  }
}

class LFMatrix[L,TF](emptyTF: =>TF) extends MatrixOps[LFMatrix[L,TF]] {
  def repr = this;
  private val map = collection.mutable.Map[L,TF]();

  private def empty = emptyTF

  def apply(label: L) = {
    map.getOrElseUpdate(label,empty);
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
        val r = new LFMatrix[L,TF](v1.empty)
        for( (l,tf) <- v1.map) {
          r(l) = op(tf,v2);
        }
        r
      }
    }
  }

  implicit def lfBinaryOp2[L,TF]
  (implicit op: BinaryOp[TF,Double,OpMul,TF], numeric: TF=>NumericOps[TF])
  : BinaryOp[LFMatrix[L,TF],Double,OpMulMatrixBy,LFMatrix[L,TF]]  = {
    new BinaryOp[LFMatrix[L,TF],Double,OpMulMatrixBy,LFMatrix[L,TF]] {
      def opType = OpMulMatrixBy;

      def apply(v1: LFMatrix[L, TF], v2: Double) = {
        val r = new LFMatrix[L,TF](v1.empty)
        for( (l,tf) <- v1.map) {
          r(l) = tf * v2;
        }
        r
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
}

object LinearClassifier {
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
    import scalanlp.stats.sampling.Rand;
    
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
