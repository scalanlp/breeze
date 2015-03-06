package breeze.numerics

import breeze.optimize._
import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.macros.expand
import breeze.math._
import spire.implicits._

package object financial {
  sealed class PaymentTime(val t: Int)
  case object Start extends PaymentTime(1)
  case object End extends PaymentTime(0)

  def futureValue(rate: Double, numPeriods: Int, payment:Double, presentValue: Double, when: PaymentTime = End):Double = {
    require(numPeriods >= 0)
    if (rate == 0) {
      -1*(presentValue+payment*numPeriods)
    } else {
      val fromPv = presentValue * math.pow(1.0+rate, numPeriods)
      val fromPayments = payment*((1.0+rate*when.t)/rate)*(math.pow(1.0+rate, numPeriods)-1.0)
      -1*(fromPv + fromPayments)
    }
  }

  def presentValue(rate: Double, numPeriods: Int, payment:Double, futureValue: Double, when: PaymentTime = End):Double = {
    require(numPeriods >= 0)
    if (rate == 0) {
      -1*(futureValue+payment*numPeriods)
    } else {
      val denominator = math.pow(1.0+rate, numPeriods)
      val fromPayments = payment*((1.0+rate*when.t)/rate)*(math.pow(1.0+rate, numPeriods)-1.0)
      -1*(futureValue + fromPayments) / denominator
    }
  }

  object netPresentValue extends UFunc {
    @expand
    implicit def reduce[@expand.args(Double, Float, Int) Scalar, T](implicit iter: CanTraverseValues[T, Scalar], @expand.sequence[Scalar](0.0, 0.0f, 0) zero: Scalar): Impl2[Double, T, Double] = new Impl2[Double, T, Double] {
      def apply(rate: Double, revenueStream: T): Double = {

        val visit = new ValuesVisitor[Scalar] {
          final val decayConst: Double = 1.0/(1.0 + rate)
          var decayUntilNow: Double = 1.0
          var sum: Double = 0.0

          def visit(a: Scalar): Unit = {
            sum += decayUntilNow*a
            decayUntilNow *= decayConst
          }

          def zeros(numZero: Int, zeroValue: Scalar): Unit = ()
        }

        iter.traverse(revenueStream, visit)

        visit.sum
      }
    }
  }

  def payment(rate: Double, numPeriods: Int, presentValue: Double, futureValue: Double = 0.0, when: PaymentTime = End):Double = {
    if (rate == 0) {
      -1*(futureValue+presentValue) / numPeriods
    } else {
      val denominator = ((1.0+rate*when.t)/rate)*(math.pow(1.0+rate, numPeriods)-1.0)
      -1*(futureValue + presentValue * math.pow(1.0+rate, numPeriods)) / denominator
    }
  }

  def principalInterest(rate: Double, numPeriods: Int, presentValue: Double, futureValue: Double = 0.0, when: PaymentTime = End): (DenseVector[Double],DenseVector[Double], DenseVector[Double]) = {
    if (when == Start) {
      throw new IllegalArgumentException("This method is broken for payment at the start of the period!")
    }
    val pmt = payment(rate, numPeriods, presentValue, futureValue, when)
    val interestPayment = DenseVector.zeros[Double](numPeriods)
    val principalPayment = DenseVector.zeros[Double](numPeriods)
    val principalRemaining = DenseVector.zeros[Double](numPeriods)
    var principal = presentValue
    var interest = presentValue*rate
    cfor(0)(i => i < numPeriods, i => i+1)(i => {
      val ip = -1*math.max(interest, 0)
      interest += ip
      principal += (pmt - ip)
      principalRemaining.unsafeUpdate(i, principal)
      interestPayment.unsafeUpdate(i, ip)
      principalPayment.unsafeUpdate(i, pmt-ip)
      interest += (principal+interest)*rate
    })
    (principalPayment, interestPayment, principalRemaining)
  }

  def interestPayments(rate: Double, numPeriods: Int, presentValue: Double, futureValue: Double = 0.0, when: PaymentTime = End): DenseVector[Double] = principalInterest(rate, numPeriods, presentValue, futureValue, when)._1

  def principalPayments(rate: Double, numPeriods: Int, presentValue: Double, futureValue: Double = 0.0, when: PaymentTime = End): DenseVector[Double] = principalInterest(rate, numPeriods, presentValue, futureValue, when)._2

  def principalRemaining(rate: Double, numPeriods: Int, presentValue: Double, futureValue: Double = 0.0, when: PaymentTime = End): DenseVector[Double] = principalInterest(rate, numPeriods, presentValue, futureValue, when)._3



  private def roots(coeffs: DenseVector[Double]) = {
    val coeffsArray = coeffs.toArray;
    val trailingZeros = coeffsArray.indexWhere(x =>
      if (0 != x) true else false);
    val tailZerosIdx = coeffsArray.lastIndexWhere(x =>
      if (0 != x) true else false)
    val nonZeroCoeffs = coeffs.slice(trailingZeros, tailZerosIdx + 1)

    val N = nonZeroCoeffs.length - 1;
    val complexRoots = if (0 < N) {
      val A = DenseMatrix.zeros[Double](N, N);
      //fill the 1th diagnal below the main diagnal with ones
      val downDiagIdxs = for(i <-(1 until N)) yield (i, i - 1)
      A(downDiagIdxs) := 1.0
      A(0 until 1, ::) := nonZeroCoeffs(1 to N) :/ -nonZeroCoeffs(0)
      val rootEig = eig(A)

      val nonZeroEigNum = rootEig.eigenvalues.length;
      val complexEig = DenseVector.zeros[Complex](nonZeroEigNum)
      for (i <- 0 until nonZeroEigNum) {
        complexEig(i) = Complex(rootEig.eigenvalues(i), rootEig.eigenvectorsComplex(i))
      }

      complexEig
    } else {
      DenseVector.zeros[Complex](N + 1)
    }
    //pading 0 to the end
    val fullRoots = if (0 < trailingZeros) {
      DenseVector.vertcat(complexRoots, DenseVector.zeros[Complex](trailingZeros))
    } else {
      complexRoots
    }
    fullRoots
  }

  def interalRateReturn(cashflow: DenseVector[Double]): Option[Double]= {
    require(cashflow(0) < 0, "Input cash flows per time period. The cashflow(0) represent the initial invesment which should be negative!")

    val res = roots(reverse(cashflow))

    val rate = if (res.length <= 0) {
      None
    } else {
      val realRes = DenseVector[Double](
        for(c:Complex <- res.toArray
            if ((c.im() == 0) && (0 < c.re())))
        yield c.re()
      )
      val rates = realRes.mapValues(v =>1.0 / v - 1.0)

      Option[Double](rates(argmin(abs(rates))))
    }

    rate
  }

  def modifiedInternalRateReturn(values:DenseVector[Double], financeRate:Double, reinvestRate:Double = 0) = {
    val n = values.length
    val positives = values.mapValues(x => if (0 < x) x else 0)
    val posCnt = values.foldLeft(0.0)((cnt, x) => if (0 < x) (cnt + 1) else cnt)
    val negatives = values.mapValues(x => if (x < 0) x else 0)
    val negCnt = values.foldLeft(0.0)((cnt, x) => if (x < 0) (cnt + 1) else cnt)
    if (posCnt == 0 || negCnt == 0) {
      throw new IllegalArgumentException("The values must has one positive and negative value!")
    }

    val inflowNPV:Double = netPresentValue(reinvestRate, positives)
    val outflowNPV:Double = netPresentValue(financeRate, negatives)
    val mirr = (pow(math.abs(inflowNPV/outflowNPV), (1.0 / (n-1))) * (1.0 + reinvestRate) - 1.0)
    mirr
  }

  def numberPeriodicPayments(rate:Double, pmt:Double,
                             pv:Double, fv:Double = 0.0, when:PaymentTime = End) = {
    require(pmt != 0, "The payment of annuity(pmt) can not be zero!")

    val nper = if (0 == rate) {
      (-fv + pv)/pmt;
    }  else {
      val z = pmt*(1.0 + rate*when.t)/rate
      log((z - fv)/(z + pv))/log(1.0 + rate)
    }
    nper
  }

  def ratePeriodicPayments(nper:Double, pmt:Double, pv:Double, fv:Double,
                           when:PaymentTime = End, guess:Double = 0.1, tol:Double = 1E-06,
                           maxiter:Int = 100) = {
    var rate = guess;
    var iter = 0
    var close = false
    while(iter < maxiter && !close) {
      val nextRate = rate - annuityFDivGradf(nper, pmt, pv, fv, when, rate)
      val diff = abs(nextRate - rate)
      close = diff < tol
      iter += 1
      rate = nextRate
    }

    if (close) Option[Double](rate) else None
  }
  //f(annuity)/f'(annuity)
  def annuityFDivGradf(nper: Double, pmt: Double, pv: Double, fv: Double, when: PaymentTime, rate: Double) = {
    val t1 = pow(1.0 + rate, nper)
    val t2 = pow(1.0 + rate, nper - 1.0)
    val annuityF = fv + pv * t1 + pmt * (t1 - 1) * (1.0 + rate * when.t) / rate
    val gradAnnuityF = nper * t2 * pv - pmt * (t1 - 1.0) * (1.0 + rate * when.t) / pow(rate, 2.0) +
      nper * pmt * t2 * (1.0 + rate * when.t) / rate + pmt * (t1 - 1) * when.t / rate
    val fDivGradF = annuityF/gradAnnuityF
    fDivGradF
  }
}
