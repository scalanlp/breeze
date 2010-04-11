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
import scalala.tensor.counters._;
import Counters._

import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.operators._;
import scalala.tensor.dense._;
import scalala.Scalala._;

import scalala.tensor.operators.TensorShapes._;

/**
 * A LinearClassifier is a multi-class classifier with decision
 * function:
 * <code>
 * \hat y_i = \arg\max_y w_y^T x_i + b_y
 * </code>
 *
 */
@serializable
@SerialVersionUID(1L)
class LinearClassifier[L,F,T2<:Tensor2[L,F] with TensorSelfOp[(L,F),T2,Shape2],
    TL<:Tensor1[L] with TensorSelfOp[L,TL,Shape1Col],
    TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]]
    (val featureWeights: T2, val intercepts: TL)
    (implicit tpb: TensorProductBuilder[T2,TF,TL,Shape2,Shape1Col,Shape1Col],
      tla: Tensor1Arith[L,TL,TL,Shape1Col])
    extends Classifier[L,TF] {
  def scores(o: TF) = {
    aggregate(featureWeights * o + intercepts);
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
