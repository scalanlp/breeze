package scalanlp.data;
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



// import scalala.tensor._;
// import scalala.tensor.counters.Counters._;
// import scalala.tensor.operators._;
// import scalala.tensor.operators.TensorShapes._;

// /**
// * Class that can provide support operations for datasets.
// * Usually used as an implicit.
// *
// * @author dlwh
// */
// trait DatasetModel[L,F,T2<:Tensor2[L,F] with TensorSelfOp[(L,F),T2,Shape2],
//       TL<:Tensor1[L] with TensorSelfOp[L,TL,Shape1Col],
//       TF<:Tensor1[F] with TensorSelfOp[F,TF,Shape1Col]] {
//   def emptyParameterMatrix(data: Seq[Example[L,TF]]):T2;
//   def emptyLabelVector(data: Seq[Example[L,TF]]): TL;
// }

// object DatasetModel {
//   implicit def counterModel[L,F] = new DatasetModel[L,F,PairedDoubleCounter[L,F],DoubleCounter[L],DoubleCounter[F]] {
//     def emptyParameterMatrix(data: Seq[Example[L,DoubleCounter[F]]]) = PairedDoubleCounter[L,F]();
//     def emptyLabelVector(data: Seq[Example[L,DoubleCounter[F]]]) = DoubleCounter[L]();
//   }
//   import scalala.tensor.dense._;
//   implicit val matrixModel : DatasetModel[Int,Int,DenseMatrix,DenseVector,DenseVector] = new MatrixModel;
  
//   private class MatrixModel extends DatasetModel[Int,Int,DenseMatrix,DenseVector,DenseVector] {
//     def emptyParameterMatrix(data: Seq[Example[Int,DenseVector]]) = {
//       val maxes:(Int,Int) = data.iterator.map(datum => (datum.label,datum.features.size)).reduceLeft { (acc,datum) =>
//         (acc._1 max datum._1,acc._2 max acc._2);
//       }

//       val (maxL,maxF) = maxes;

//       new DenseMatrix(maxL+1,maxF);
//     }
//     def emptyLabelVector(data: Seq[Example[Int,DenseVector]]) = {
//       val maxL = data.foldLeft(0) { (acc,datum) =>
//         (acc max datum.label)
//       }

//       new DenseVector(maxL+1);
//     }
//   }
// }

