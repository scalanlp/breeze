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

import breeze.linalg.{DenseMatrix, DenseVector, norm}
import breeze.numerics._
import breeze.optimize.OptimizeTestBase

class NNLSTest extends OptimizeTestBase {
  /** Generate an NNLS problem whose optimal solution is the all-ones vector. */
  def genOnesData(n: Int): (DenseMatrix[Double], DenseVector[Double]) = {
    val A = DenseMatrix.rand[Double](n, n)
    val b = A*DenseVector.ones[Double](n)
    
    val ata = A.t*A
    val atb = A.t*b

    (ata, atb)
  }
  
  test("NNLS: exact solution cases") {
    val n = 20
    val nnls = new NNLS()
    var numSolved = 0
    
    // About 15% of random 20x20 [-1,1]-matrices have a singular value less than 1e-3.  NNLS
    // can legitimately fail to solve these anywhere close to exactly.  So we grab a considerable
    // sample of these matrices and make sure that we solved a substantial fraction of them.    
    for (k <- 0 until 100) {
      val (ata, atb) = genOnesData(n)
      val x = nnls.minimize(ata, atb)
      atb *= -1.0
      val golden = DenseVector.ones[Double](n)
      x -= golden
      if ((norm(x, 2) < 1e-2) && (norm(x, inf) < 1e-3)) numSolved = numSolved + 1
    }
    assert(numSolved > 50)
  }
  
  test("NNLS: nonnegativity constraint active") {
    val n = 5
    val ata = new DenseMatrix[Double](5, 5, 
        Array(4.377, -3.531, -1.306, -0.139,  3.418,
            -3.531,  4.344,  0.934,  0.305, -2.140,
            -1.306,  0.934,  2.644, -0.203, -0.170,
            -0.139,  0.305, -0.203,  5.883,  1.428,
            3.418, -2.140, -0.170,  1.428,  4.684))
            
    val atb = DenseVector[Double](-1.632, 2.115, 1.094, -1.025, -0.636)
    
    val goodx = Array(0.13025, 0.54506, 0.2874, 0.0, 0.028628)

    val nnls = new NNLS()
    val x = nnls.minimize(ata, atb)
    for (i <- 0 until n) {
      assert(abs(x(i) - goodx(i)) < 1E-3)
      assert(x(i) >= 0)
    }
  }
  
  test("NNLS: objective value test") {
    val n = 5
    val ata = new DenseMatrix[Double](5, 5,
      Array(517399.13534, 242529.67289, -153644.98976, 130802.84503, -798452.29283
      , 242529.67289, 126017.69765, -75944.21743, 81785.36128, -405290.60884
      , -153644.98976, -75944.21743, 46986.44577, -45401.12659, 247059.51049
      , 130802.84503, 81785.36128, -45401.12659, 67457.31310, -253747.03819
      , -798452.29283, -405290.60884, 247059.51049, -253747.03819, 1310939.40814)
      )
      
    val atb = DenseVector(-31755.05710, 13047.14813, -20191.24443, 25993.77580, 11963.55017)

    /** reference solution obtained from matlab function quadprog */
    val refx = DenseVector(34.90751, 103.96254, 0.00000, 27.82094, 58.79627)
    val refObj = NNLS.computeObjectiveValue(ata, atb, refx)
    
    val nnls = new NNLS()
    val x = nnls.minimize(ata, atb)
    val obj = NNLS.computeObjectiveValue(ata, atb, x)
    
    assert(obj < refObj + 1E-5)
  }
}
