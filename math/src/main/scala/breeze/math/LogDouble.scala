package breeze
package math

/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

import scala.math._
import breeze.linalg.{softmax, logDiff}

/**
 * Represents a double in log space, to prevent under/overflow
 * These guys are horribly slow right now, thanks to boxing in Java. Hopefully that will go away one day.
 *
 * @author dlwh
 */
class LogDouble(val logValue: Double) {
  def value = exp(logValue)

  def *(other: LogDouble) = new LogDouble(logValue + other.logValue)

  def /(other: LogDouble) = new LogDouble(logValue - other.logValue)

  def +(other: LogDouble) = new LogDouble(softmax(logValue, other.logValue))

  def -(other: LogDouble) = new LogDouble(logDiff(logValue, other.logValue))

  def *(d: Double) = new LogDouble(logValue + log(d))

  def /(d: Double) = new LogDouble(logValue - log(d))

  def +(d: Double) = new LogDouble(softmax(logValue, log(d)))

  def -(d: Double) = new LogDouble(logDiff(logValue, log(d)))

  override def toString = "LogDouble(" + logValue + ")"

  override def equals(o: Any) =
    o match {
      case ld: LogDouble => logValue == ld.logValue
      case _ => false
    }

  override def hashCode = logValue.hashCode
}

object LogDouble {
  implicit def doubleExtra(d: Double) =
    new {

      /**
       * Assumes the double is already logged.
       */
      def asLogDouble = new LogDouble(d)

      /**
       * Stores the double in LogSpace
       */
      def toLogDouble = new LogDouble(scala.math.log(d))

      def logValue = scala.math.log(d)

      // Operations assume the Double is in "Normal" space
      def *(o: LogDouble) = new LogDouble(o.logValue + scala.math.log(d))

      def /(o: LogDouble) = new LogDouble(scala.math.log(d) - o.logValue)

      def +(o: LogDouble) = new LogDouble(softmax(o.logValue, scala.math.log(d)))

      def -(o: LogDouble) = new LogDouble(logDiff(scala.math.log(d), o.logValue))
    }

  implicit def logDoubleToDouble(d: LogDouble) = d.value

  def log(d: LogDouble) = new LogDouble(scala.math.log(d.logValue))

  def exp(d: LogDouble) = new LogDouble(d.value)

  def pow(d: LogDouble, p: Double) = new LogDouble(d.logValue * p)

  def pow(d: LogDouble, p: LogDouble) = new LogDouble(d.logValue * p.value)

  implicit object SemiringLogDouble extends Semiring[LogDouble] {
    def zero: LogDouble = new LogDouble(Double.NegativeInfinity)

    def one: LogDouble = new LogDouble(0)

    def +(a: LogDouble, b: LogDouble): LogDouble = a + b

    def *(a: LogDouble, b: LogDouble): LogDouble = a * b

    def ==(a: LogDouble, b: LogDouble): Boolean = a == b

    def !=(a: LogDouble, b: LogDouble): Boolean = a != b
  }
}
