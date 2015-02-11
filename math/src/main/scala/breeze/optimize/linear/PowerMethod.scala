/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package breeze.optimize.linear

import breeze.linalg.operators.OpMulMatrix
import breeze.math.{MutableInnerProductModule}
import breeze.numerics.abs
import breeze.util.SerializableLogging
import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.util.Implicits._
import breeze.optimize.proximal.QuadraticMinimizer
/**
 * Created by debasish83 on 2/3/15.
 */
class PowerMethod[T, M](maxIterations: Int = 10,tolerance: Double = 1E-5, inverse: Boolean = false)
                       (implicit space: MutableInnerProductModule[T, Double],
                        mult: OpMulMatrix.Impl2[M, T, T]) extends SerializableLogging {

  import space._

  case class State(eigenValue: Double, eigenVector: T, iter: Int, converged: Boolean)

  def initialState(y: T, A: M): State = {
    //Force y to be a vector of unit norm
    val normInit = norm(y)
    y *= 1.0 / normInit
    //TO DO : How to fix the asInstanceOf, at least throw a exception/require
    val ay = if (inverse)
      QuadraticMinimizer.solveTriangular(A.asInstanceOf[DenseMatrix[Double]], y.asInstanceOf[DenseVector[Double]]).asInstanceOf[T]
    else mult(A, y)

    val lambda = y dot ay

    y := ay
    val norm1 = norm(ay)
    y *= 1.0/norm1
    if (lambda < 0.0) y *= -1.0
    State(lambda, y, 0, false)
  }

  def iterations(y: T,
                 A: M): Iterator[State] = Iterator.iterate(initialState(y, A)) { state =>
    import state._
    val ay = if (inverse)
      QuadraticMinimizer.solveTriangular(A.asInstanceOf[DenseMatrix[Double]], eigenVector.asInstanceOf[DenseVector[Double]]).asInstanceOf[T]
      else mult(A, eigenVector)
    val lambda = eigenVector dot ay
    val norm1 = norm(ay)
    ay *= 1.0 / norm1
    if (lambda < 0.0) ay *= -1.0

    val val_dif = abs(lambda - eigenValue)
    if (val_dif <= tolerance || iter > maxIterations) State(lambda, ay, iter + 1, true)
    else State(lambda, ay, iter + 1, false)
  }.takeUpToWhere(_.converged)

  def iterateAndReturnState(y: T, A: M): State = {
    iterations(y, A).last
  }

  def eigen(y: T, A: M): Double = {
    iterateAndReturnState(y, A).eigenValue
  }
}
