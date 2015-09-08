/*
 *
 *  Copyright 2015 David Hall
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

import breeze.benchmark.{MyRunner, BreezeBenchmark}
import spire.syntax.cfor._

/**
 * Created by dlwh on 8/14/15.
 */
class DenseMulScalarBenchmark extends BreezeBenchmark {
  assert(usingNatives)

  val dv, dv2 = DenseVector.rand[Double](10000)

  def timeSmallDVMulScalar(reps: Int) = {
    var sum = 0.0
    var q = dv2
    cforRange(0 until reps) { rep =>
      q = dv :* q
    }
    q
  }

  def timeSmallDVInlineRange(reps: Int) = {
    var result = new Array[Double](dv.length)
    var b = dv2
    cforRange(0 until reps) { rep =>
      val ad = dv.data
      val bd = b.data
      cforRange(0 until dv.length) { i =>
        result(i) = ad(i) * bd(i)
      }
      b = new DenseVector(result)
      result = new Array[Double](dv.length)
    }
    b
  }

  def timeSmallDVScaleAddInline(reps: Int) = {
    val dv = this.dv.data
    var dv2 = this.dv2.data
    var result = new Array[Double](dv.length)
    cforRange(0 until reps) { rep =>
      result = new Array[Double](dv.length)
      result(0) += dv2(0) * dv(0)
      result(1) += dv2(1) * dv(1)
      result(2) += dv2(2) * dv(2)
      result(3) += dv2(3) * dv(3)
      result(4) += dv2(4) * dv(4)
      result(5) += dv2(5) * dv(5)
      result(6) += dv2(6) * dv(6)
      result(7) += dv2(7) * dv(7)
      result(8) += dv2(8) * dv(8)
      result(9) += dv2(9) * dv(9)
      dv2 = result
    }
    result
  }

}



object DenseMulScalarBenchmark extends MyRunner(classOf[DenseMulScalarBenchmark])
