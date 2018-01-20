package breeze.numerics.financial

/*
 Copyright 2012 David Hall

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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

import breeze.linalg._

/**
 *
 * @author stucchio
 */
@RunWith(classOf[JUnitRunner])
class FinancialTest extends FunSuite {

  val DOUBLE_ROUND5_MIN = 1E-5;

  test("NetPresentValue") {
    assert(netPresentValue(1.0, Seq(1)) == 1.0)
    assert(netPresentValue(1.0, Seq(1, 1)) == 1.5)
    assert(netPresentValue(1.0, Seq(1, 1, 1)) == 1.75)
    assert(netPresentValue(1.0, Seq(1, 1, 2)) == 2.0)
  }
  test("FutureValue") {
    assert(math.abs(futureValue(0.05 / 12, 10 * 12, -100, -100) - 15692.92889) < 1e-5)
    assert(futureValue(0.0, 3, 1, 1) == -4.0)
  }
  test("presentValue") {
    assert(math.abs(presentValue(0.05 / 12, 10 * 12, -100, 15692.93) - -100.0006713) < 1e-5)
    assert(presentValue(0, 3, 1, 1) == -4.0)
  }
  test("payment") {
    assert(math.abs(payment(0.075 / 12, 12 * 15, 200000, 0) - -1854.0247200054619) < 1e-5)
    assert(math.abs(payment(0.0, 10, 1100, -100) - -100.0) < 1e-5)
  }

  test("principal vs interest payments") {
    val (principal, interest, remainingPrincipal) = principalInterest(0.0824 / 12, 12, 2500.0)

    val expectedRemainingPrincipal = DenseVector[Double](2299.42, 2097.46, 1894.11, 1689.37, 1483.22, 1275.66, 1066.67,
      856.25, 644.38, 431.05, 216.26, -0.00)
    val expectedInterestPayment =
      DenseVector[Double](-17.17, -15.79, -14.40, -13.01, -11.60, -10.18, -8.76, -7.32, -5.88, -4.42, -2.96, -1.49)
    val expectedPrincipalPayment = DenseVector[Double](-200.58, -201.96, -203.35, -204.74, -206.15, -207.56, -208.99,
      -210.42, -211.87, -213.32, -214.79, -216.26)

    assert(norm(expectedRemainingPrincipal - remainingPrincipal) < 2e-1)
    assert(norm(expectedPrincipalPayment - principal) < 1e-1)
    assert(norm(expectedInterestPayment - interest) < 1e-1)
  }

  test("IRR vs MIRR") {

    //Some(0.2809484211599611)
    var expectNormalIRR = DenseVector[Double](-100, 39, 59, 55, 20);
    assert(math.abs(interalRateReturn(expectNormalIRR).get - 0.2809484211599611) < DOUBLE_ROUND5_MIN)
    //Some(0.08859833852775578)
    expectNormalIRR = DenseVector[Double](-5, 10.5, 1, -8, 1);
    assert(math.abs(interalRateReturn(expectNormalIRR).get - 0.08859833852775578) < DOUBLE_ROUND5_MIN)
    //Some(-0.09549583034897258)
    val expectNegativeIRR = DenseVector[Double](-100, 0, 0, 74)
    assert(math.abs(interalRateReturn(expectNegativeIRR).get + 0.09549583034897258) < DOUBLE_ROUND5_MIN)

    val expectZeroIRR = DenseVector[Double](-2000, 500, 500, 1000)
    assert(math.abs(interalRateReturn(expectZeroIRR).get) < DOUBLE_ROUND5_MIN)

    val expectNormalMIRR = modifiedInternalRateReturn(DenseVector[Double](-180, 42, 39, 40, 32, 48), 0.08, 0.11);
    assert(math.abs(expectNormalMIRR - 0.06782) < DOUBLE_ROUND5_MIN)
    val expectNegativeMIRR = modifiedInternalRateReturn(DenseVector[Double](-180, 42, 39, 40), 0.08, 0.11)
    assert(math.abs(expectNegativeMIRR + 0.091354) < DOUBLE_ROUND5_MIN)
    val withMultiNegMIRR = modifiedInternalRateReturn(DenseVector[Double](-180, 42, 39, -50, 40, 32, 48), 0.08, 0.11);
    assert(math.abs(withMultiNegMIRR - 0.0303) < DOUBLE_ROUND5_MIN)
  }

  test("number periodic vs solve rate of annuity") {

    val normalRate = 0.07 / 12 //~0.00583191332402286
    val expectNormal = numberPeriodicPayments(normalRate, -150, 8000)
    //expect 64.07335
    assert(math.abs(expectNormal - 64.07335) < DOUBLE_ROUND5_MIN)
    //expect -53.33333
    val testZeroRate = numberPeriodicPayments(0, -150, 8000)
    assert(math.abs(testZeroRate - -53.33333) < DOUBLE_ROUND5_MIN)

    val expectNormalRate = ratePeriodicPayments(expectNormal, -150, 8000, 0).get
    assert(math.abs(expectNormalRate - normalRate) < DOUBLE_ROUND5_MIN)
  }
}
