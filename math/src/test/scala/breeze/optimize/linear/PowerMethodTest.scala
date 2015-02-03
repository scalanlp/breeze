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

import breeze.numerics.abs
import breeze.stats.distributions.Rand
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import breeze.linalg._
import breeze.optimize.proximal.QpGenerator

/**
 * Created by v606014 on 2/3/15.
 */
@RunWith(classOf[JUnitRunner])
class PowerMethodTest extends FunSuite {
  val n = 10
  val gram = QpGenerator.getGram(n)
  val eigs = eigSym(gram)

  test("max eigen value from power method approximately equal to eigSym") {
    val eigenGold = max(eigs.eigenvalues)
    val pm = new PowerMethod[DenseVector[Double], DenseMatrix[Double]]()
    val init = DenseVector.rand[Double](n, Rand.gaussian(0, 1))
    val eigenApprox = pm.eigen(init, gram)
    println(s"Max Eigen test approx $eigenApprox gold $eigenGold")
    assert(abs(eigenGold - eigenApprox) < 1e-3)
  }

  test("power method and min eigen value computation generate similar result") {
    val eigenGold = min(eigs.eigenvalues)
    val pm = new PowerMethod[DenseVector[Double], DenseMatrix[Double]]()
    val inverseGram = gram \ DenseMatrix.eye[Double](gram.rows)
    val init = DenseVector.rand[Double](n, Rand.gaussian(0, 1))
    val eigenApprox = 1.0/pm.eigen(init, inverseGram)
    println(s"Min Eigen test approx $eigenApprox gold $eigenGold")
    assert(abs(eigenGold - eigenApprox) < 1e-3)
  }
}
