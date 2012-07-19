package breeze.numerics

/**
 * Scaling utilities.
 *
 * Often, in order to avoid underflow, we can offload some of the
 * exponent of a double into an int. To make things more efficient,
 * we can actually share that exponent between doubles.
 *
 * The scales used in this trait are in log space: they can
 * be safely added and subtracted.
 *
 * @author dlwh
 */
trait Scaling {
  /**
   *
   * the largest (log) power of two we want to deal with
   */
  val scaleConstant: Int

  /**
   * Ensures that all doubles are between (2**scaleConstant,2**-scaleConstant),
   * scaling the array as necessary. If that's not possible, we prefer
   * to keep the largest values less than 2**scaleConstant
   * @param scores
   * @param currentScale
   * @return newScale
   */
  def scaleArray(scores: Array[Double], currentScale: Int):Int = {
    val scaleDelta = computeScaleDelta(scores)

    if(scaleDelta != 0) {
      var i = 0
      while(i < scores.length) {
        scores(i) = java.lang.Math.scalb(scores(i), scaleDelta)
        i += 1
      }
    }

    currentScale + scaleDelta
  }

  /**
   * Computes the log power of two we'd need to scale by
   * so that all doubles are between (2 ** scaleConstant, 2 ** -scaleConstant).
   *
   * If it's not possible to represent all numbers in that range, we prefer
   * to keep all doubles less than 2 ** scaleConstant, letting others
   * underflow
   *
   * @param scores
   * @return
   */
  def computeScaleDelta(scores: Array[Double]):Int = {
    var maxScale = 0
    var minScale = 0
    var i = 0
    while(i < scores.length) {
      val exp = java.lang.Math.getExponent(scores(i))
      maxScale = math.max(maxScale, exp)
      minScale = math.min(minScale, exp)
      i += 1
    }

    // if we scalb by this value, then all doubles will be in the range we want.
    // note that minScale < 0 in general
    if(maxScale > scaleConstant) -scaleConstant * (maxScale / scaleConstant)
    else if(minScale < -scaleConstant) -scaleConstant * (minScale / scaleConstant)
    else 0
  }

  def scaleArrayToScale(scores: Array[Double], currentScale: Int, targetScale: Int) {
    val scaleDelta = targetScale - currentScale

    if(scaleDelta != 0) {
      var i = 0
      while(i < scores.length) {
        scores(i) = java.lang.Math.scalb(scores(i), scaleDelta)
        i += 1
      }
    }

  }

  /**
   * Converts the scaled value into "normal" space
   */
  def unscaleValue(score: Double, currentScale: Int) = {
    java.lang.Math.scalb(score, -currentScale)
  }

}

object Scaling extends Scaling {
  val scaleConstant = 145 // 10^63 more or less
}

