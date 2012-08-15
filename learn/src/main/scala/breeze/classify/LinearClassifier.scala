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
import breeze.linalg._
import breeze.linalg.operators._
import breeze.math.{MutableCoordinateSpace, TensorSpace, VectorSpace}
import breeze.linalg.support.{CanCopy, CanZipMapValues, CanNorm, CanCreateZerosLike}
import breeze.generic.{URFunc, UReduceable, CanMapValues}
import breeze.util.{Index, MutableIndex}
import collection.mutable.ArrayBuffer

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
     mulTensors : BinaryOp[T2,TF,OpMulMatrix,TL],
     view: TL <:< QuasiTensor[L, Double]) extends Classifier[L,TF] with Serializable {
  import vspace._
  def scores(o: TF) = {
    val r = featureWeights * o + intercepts
    val ctr = Counter[L, Double]()
    for((l, v) <- r.iterator) {
      ctr(l) = v
    }
    ctr
  }
}

object LinearClassifier {
  implicit def linearClassifierReadWritable[L, T2, TL, TF](implicit viewT2 : T2<:<NumericOps[T2], vspace: VectorSpace[TL, Double],
                                                        mulTensors : BinaryOp[T2,TF,OpMulMatrix,TL],
                                                        view: TL <:< Tensor[L, Double],
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


