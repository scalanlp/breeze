/*
 *
 *  Copyright 2016 David Hall
 *
 *  Licensed under the Apache License, Version 2.0 (the "License")
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * /
 */

package breeze.linalg

import breeze.math.Semiring
import breeze.storage.Zero
import org.scalacheck.{Arbitrary, Gen}

import scala.reflect.ClassTag

/**
 * Code for generating test examples for use with scalacheck etc
 *
 * @author dlwh
  **/
object RandomInstanceSupport {

  // relative errors get really screwy for small and big values
  def reasonableClamp(v: Double, lower: Double = 1E-4, upper: Double = 1E5): Double = {
    if (v == 0) 0
    else if (v.abs < lower) v + math.signum(v) * lower
    else if (v.abs > upper) reasonableClamp(v % upper)
    else v
  }

  val genReasonableDouble: Arbitrary[Double] = Arbitrary {
    Arbitrary.arbitrary[Double].map(reasonableClamp(_))
  }

  val genReasonableFloat: Arbitrary[Float] = Arbitrary {
    Arbitrary.arbitrary[Double].map(reasonableClamp(_, 1E-4, 1E4).toFloat)
  }

  val genReasonableInt: Arbitrary[Int] = Arbitrary {
    Gen.Choose.chooseInt.choose(-1000, 1000)
  }

  def genVector[T: ClassTag: Zero: Semiring](len: Int, gen: Gen[T]): Gen[Vector[T]] = {
    Gen.oneOf(genDenseVector(len, gen), genSparseVector(len, gen), genHashVector(len, gen))
  }

  def genDenseVector[T: ClassTag](len: Int, gen: Gen[T]): Gen[DenseVector[T]] =
    for {
      offset <- Gen.choose(0, 10)
      stride <- Gen.choose(1, 4)
      list <- Gen.listOfN(len * stride + offset, gen)
    } yield {
      DenseVector(list: _*).apply(offset until (len * stride + offset) by stride)
    }

  def genSparseVector[T: ClassTag: Zero: Semiring](len: Int, gen: Gen[T]): Gen[SparseVector[T]] =
    genVectorBuilder(len, gen).map(_.toSparseVector)

  def genHashVector[T: ClassTag: Zero](len: Int, gen: Gen[T]): Gen[HashVector[T]] =
    for {
      nnz <- Gen.choose(0, len)
      il <- Gen.listOfN(nnz, Gen.choose(0, len - 1))
      list <- Gen.listOfN(nnz, gen)
    } yield {
      HashVector(len)(il.zip(list): _*)
    }

  def genVectorBuilder[T: ClassTag: Semiring](len: Int, gen: Gen[T]): Gen[VectorBuilder[T]] =
    for {
      nnz <- Gen.choose(0, len)
      il <- Gen.listOfN(nnz, Gen.choose(0, len - 1))
      list <- Gen.listOfN(nnz, gen)
    } yield {
      VectorBuilder(len)(il.zip(list): _*)
    }
}
