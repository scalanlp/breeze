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
}
