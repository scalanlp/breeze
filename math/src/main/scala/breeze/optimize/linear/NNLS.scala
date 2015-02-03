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

import breeze.linalg.{DenseMatrix, DenseVector, axpy}
import breeze.optimize.proximal.QpGenerator
import breeze.stats.distributions.Rand
import breeze.util.Implicits._
/**
 * Object used to solve nonnegative least squares problems using a modified
 * projected gradient method.
 */

class NNLS(val maxIters: Int = -1) {
  type BDM = DenseMatrix[Double]
  type BDV = DenseVector[Double]

  case class State(x: BDV, grad: BDV, dir: BDV, lastDir: BDV, res: BDV,
                   lastNorm: Double, lastWall: Int, iter: Int, converged: Boolean)

  // find the optimal unconstrained step
  def steplen(ata: BDM, dir: BDV, res: BDV,
              tmp: BDV): Double = {
    val top = dir.dot(res)
    tmp := ata * dir
    // Push the denominator upward very slightly to avoid infinities and silliness
    top / (tmp.dot(dir) + 1e-20)
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

  /**
   * Solve a least squares problem, possibly with nonnegativity constraints, by a modified
   * projected gradient method.  That is, find x minimising ||Ax - b||_2 given A^T A and A^T b.
   *
   * We solve the problem
   * min_x      1/2 x' ata x' - x'atb
   * subject to x >= 0
   *
   * The method used is similar to one described by Polyak (B. T. Polyak, The conjugate gradient
   * method in extremal problems, Zh. Vychisl. Mat. Mat. Fiz. 9(4)(1969), pp. 94-112) for bound-
   * constrained nonlinear programming.  Polyak unconditionally uses a conjugate gradient
   * direction, however, while this method only uses a conjugate gradient direction if the last
   * iteration did not cause a previously-inactive constraint to become active.
   */
  def initialState(ata: DenseMatrix[Double], atb: DenseVector[Double], n: Int): State = {
    require(ata.cols == ata.rows, s"NNLS:iterations gram matrix must be symmetric")
    require(ata.rows == atb.length, s"NNLS:iterations gram matrix rows must be same as length of linear term")

    val grad = DenseVector.zeros[Double](n)
    val x = DenseVector.zeros[Double](n)
    val dir = DenseVector.zeros[Double](n)
    val lastDir = DenseVector.zeros[Double](n)
    val res = DenseVector.zeros[Double](n)
    val lastNorm = 0.0
    val lastWall = 0
    State(x, grad, dir, lastDir, res, lastNorm, lastWall, 0, false)
  }

  def iterations(ata: DenseMatrix[Double],
                 atb: DenseVector[Double]): Iterator[State] =
    Iterator.iterate(initialState(ata, atb, atb.length)) { state =>
      import state._
      val n = atb.length
      val tmp = DenseVector.zeros[Double](atb.length)

      val iterMax = if (maxIters < 0) Math.max(400, 20 * n) else maxIters

      // find the residual
      res := ata * x
      res -= atb
      grad := res

      // project the gradient
      var i = 0
      while (i < n) {
        if (grad.data(i) > 0.0 && x.data(i) == 0.0) {
          grad.data(i) = 0.0
        }
        i = i + 1
      }
      val ngrad = grad.dot(grad)
      dir := grad

      // use a CG direction under certain conditions
      var step = steplen(ata, grad, res, tmp)
      var ndir = 0.0
      val nx = x.dot(x)

      if (iter > lastWall + 1) {
        val alpha = ngrad / lastNorm
        axpy(alpha, lastDir, dir)
        val dstep = steplen(ata, dir, res, tmp)
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
      if (stop(step, ndir, nx) || iter > iterMax)
        State(x, grad, dir, lastDir, res, lastNorm, lastWall, iter + 1, true)
      else {
        // don't run through the walls
        i = 0
        while (i < n) {
          if (step * dir.data(i) > x.data(i)) {
            step = x.data(i) / dir.data(i)
          }
          i = i + 1
        }

        var nextWall = lastWall

        // take the step
        i = 0
        while (i < n) {
          if (step * dir.data(i) > x.data(i) * (1 - 1e-14)) {
            x.data(i) = 0
            nextWall = iter
          } else {
            x.data(i) -= step * dir.data(i)
          }
          i = i + 1
        }
        lastDir := dir
        val nextNorm = ngrad
        State(x, grad, dir, lastDir, res, nextNorm, nextWall, iter + 1, false)
      }
    }.takeUpToWhere(_.converged)

  def minimizeAndReturnState(ata: DenseMatrix[Double],
                             atb: DenseVector[Double]): State = {
    iterations(ata, atb).last
  }

  def minimize(ata: DenseMatrix[Double], atb: DenseVector[Double]): DenseVector[Double] = {
    minimizeAndReturnState(ata, atb).x
  }
}

object NNLS {
  /** Compute the objective value */
  def computeObjectiveValue(ata: DenseMatrix[Double], atb: DenseVector[Double], x: DenseVector[Double]): Double = {
    val res = (x.t*ata*x)*0.5 - atb.dot(x)
    res
  }

  def apply(iters: Int) = new NNLS(iters)

  def main(args: Array[String]) {
    if (args.length < 2) {
      println("Usage: NNLS n s")
      println("Test NNLS with quadratic function of dimension n for s consecutive solves")
      sys.exit(1)
    }

    val problemSize = args(0).toInt
    val numSolves = args(1).toInt
    val nnls = new NNLS()

    var i = 0
    var nnlsTime = 0L
    while(i < numSolves) {
      val ata = QpGenerator.getGram(problemSize)
      val atb = DenseVector.rand[Double](problemSize, Rand.gaussian(0,1))
      val startTime = System.nanoTime()
      nnls.minimize(ata, atb)
      nnlsTime = nnlsTime + (System.nanoTime() - startTime)
      i = i + 1
    }
    println(s"NNLS problemSize $problemSize solves $numSolves ${nnlsTime/1e6} ms")
  }
}
