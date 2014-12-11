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

package breeze.optimize.quadratic

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg.axpy
import breeze.linalg.support.CanCopy
import breeze.linalg.copy
import breeze.linalg.operators.OpMulMatrix
import com.github.fommil.netlib.BLAS.{getInstance => blas}

/**
 * Object used to solve nonnegative least squares problems using a modified
 * projected gradient method.
 */

class NNLS(val n: Int)(implicit mult: OpMulMatrix.Impl2[DenseMatrix[Double], DenseVector[Double], DenseVector[Double]]) {
  val scratch = DenseVector.zeros[Double](n)
    val grad = DenseVector.zeros[Double](n)
    val x =  DenseVector.zeros[Double](n)
    val dir = DenseVector.zeros[Double](n)
    val lastDir = DenseVector.zeros[Double](n)
    val res = DenseVector.zeros[Double](n)
    
    var iterations = 0
    var solveTime = 0L
    
    def wipe() {
      scratch := 0.0
      grad := 0.0
      x := 0.0
      dir := 0.0
      lastDir := 0.0
      res := 0.0
    }
  
  /**
   * Solve a least squares problem, possibly with nonnegativity constraints, by a modified
   * projected gradient method.  That is, find x minimising ||Ax - b||_2 given A^T A and A^T b.
   *
   * We solve the problem
   *   min_x      1/2 x^T ata x^T - x^T atb
   *   subject to x >= 0
   *
   * The method used is similar to one described by Polyak (B. T. Polyak, The conjugate gradient
   * method in extremal problems, Zh. Vychisl. Mat. Mat. Fiz. 9(4)(1969), pp. 94-112) for bound-
   * constrained nonlinear programming.  Polyak unconditionally uses a conjugate gradient
   * direction, however, while this method only uses a conjugate gradient direction if the last
   * iteration did not cause a previously-inactive constraint to become active.
   */
  def solve(ata: DenseMatrix[Double], atb: DenseVector[Double]) : Array[Double] = {
    wipe()
    
    val solveStart = System.nanoTime()
    val n = atb.length
    
    // find the optimal unconstrained step
    def steplen(dir: DenseVector[Double], res: DenseVector[Double]): Double = {
      val top = dir.dot(res) //SimpleBlas.dot(dir, res)
      scratch := mult(ata, dir)
      //SimpleBlas.gemv(1.0, ata, dir, 0.0, scratch)
      // Push the denominator upward very slightly to avoid infinities and silliness
      top / (scratch.dot(dir) /*SimpleBlas.dot(scratch, dir)*/ + 1e-20)
    }

    // stopping condition
    def stop(step: Double, ndir: Double, nx: Double): Boolean = {
        ((step.isNaN) // NaN
      || (step < 1e-7) // too small or negative
      || (step > 1e40) // too small; almost certainly numerical problems
      || (ndir < 1e-12 * nx) // gradient relatively too small
      || (ndir < 1e-32) // gradient absolutely too small; numerical issues may lurk
      )
    }
    
    val iterMax = Math.max(400, 20 * n)
    var lastNorm = 0.0
    var iterno = 0
    var lastWall = 0 // Last iteration when we hit a bound constraint.
    var i = 0
    while (iterno < iterMax) {
      // find the residual
      res := mult(ata, x)
      axpy(-1.0, atb, res)
      grad := res      
      
      // project the gradient
      i = 0
      while (i < n) {
        if (grad.data(i) > 0.0 && x.data(i) == 0.0) {
          grad.data(i) = 0.0
        }
        i = i + 1
      }
      val ngrad = grad.dot(grad)
      dir := grad
          
      // use a CG direction under certain conditions
      var step = steplen(grad, res)
      var ndir = 0.0
      val nx = x.dot(x)
      
      if (iterno > lastWall + 1) {
        val alpha = ngrad / lastNorm
        axpy(alpha, lastDir, dir)
        val dstep = steplen(dir, res)
        ndir = dir.dot(dir)
        if (stop(dstep, ndir, nx)) {
          // reject the CG step if it could lead to premature termination
          dir := grad
          ndir = dir.dot(dir)
        } else {
          step = dstep
        }
      } else {
        ndir = dir.dot(dir)
      }

      // terminate?
      if (stop(step, ndir, nx)) {
        iterations += iterno
        solveTime += System.nanoTime() - solveStart
        return x.data.clone
      }
      
      // don't run through the walls
      i = 0
      while (i < n) {
        if (step * dir.data(i) > x.data(i)) {
          step = x.data(i) / dir.data(i)
        }
        i = i + 1
      }

      // take the step
      i = 0
      while (i < n) {
        if (step * dir.data(i) > x.data(i) * (1 - 1e-14)) {
          x.data(i) = 0
          lastWall = iterno
        } else {
          x.data(i) -= step * dir.data(i)
        }
        i = i + 1
      }

      iterno = iterno + 1
      lastDir := dir
      lastNorm = ngrad
    }
    
    iterations += iterno
    solveTime += System.nanoTime() - solveStart
    x.data.clone
  }
}

object NNLS {
  /** Compute the objective value */
  def computeObjectiveValue(ata: DenseMatrix[Double], atb: DenseVector[Double], x: DenseVector[Double]): Double = {
    val res = (x.t*ata*x)*0.5 - atb.dot(x)
    res
  }
  
  def apply(n: Int) = new NNLS(n)
}
