package breeze.linalg

import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

trait VectorMatchers {

  class VectorsSimilar(right: DenseVector[Double], allowedDeviation: Double = 0.0, normP: Double = 2.0) extends Matcher[DenseVector[Double]] {

    def apply(left: DenseVector[Double]) = {
      val deviation = norm((left - right), normP)
      val failureMessageSuffix = s"vector $left deviates by $deviation from $right, expected deviation <= $allowedDeviation (norm = $norm)"

      val negatedFailureMessageSuffix = s"vector $left deviates by $deviation from $right, expected deviation > $allowedDeviation (norm = $norm)"

      MatchResult(
        deviation <= allowedDeviation,
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }

  def beSimilarTo(right: DenseVector[Double], allowedDeviation: Double = 0.0, norm: Double = 2.0) = new VectorsSimilar(right, allowedDeviation, norm)
}

object VectorMatchers extends VectorMatchers
