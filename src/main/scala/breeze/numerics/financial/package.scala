package breeze.numerics

import breeze.generic.UFunc
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.macros.expand

package object financial {
  sealed trait PaymentTime
  case object Start extends PaymentTime
  case object End extends PaymentTime

  //Future Value
  def futureValue(rate: Double, numPeriods: Int, payment:Double, presentValue: Double, when: PaymentTime = End):Double = {
    if (rate == 0) {
      -1*(presentValue+payment*numPeriods)
    } else {
      val fromPv = presentValue * math.pow(1.0+rate, numPeriods)
      val fromPayments = when match {
        case Start => payment*((1.0+rate)/rate)*(math.pow(1.0+rate, numPeriods)-1.0)
        case End => payment*(1.0/rate)*(math.pow(1.0+rate, numPeriods)-1.0)
      }
      -1*(fromPv + fromPayments)
    }
  }

  def presentValue(rate: Double, numPeriods: Int, payment:Double, futureValue: Double, when: PaymentTime = End):Double = {
    if (rate == 0) {
      -1*(futureValue+payment*numPeriods)
    } else {
      val denominator = math.pow(1.0+rate, numPeriods)
      val fromPayments = when match {
        case Start => payment*((1.0+rate)/rate)*(math.pow(1.0+rate, numPeriods)-1.0)
        case End => payment*(1.0/rate)*(math.pow(1.0+rate, numPeriods)-1.0)
      }
      -1*(futureValue + fromPayments) / denominator
    }
  }

  //Net Present Value
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

}
