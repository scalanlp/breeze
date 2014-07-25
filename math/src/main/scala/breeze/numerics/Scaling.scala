package breeze.numerics

import breeze.linalg.axpy

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
   * Ensures that the max double is between (2**scaleConstant,2**-scaleConstant),
   * scaling the array as necessary.
   * @param scores
   * @param currentScale
   * @return newScale
   */
  def scaleArray(scores: Array[Double], currentScale: Int):Int = {
    val scaleDelta = computeScaleDelta(scores)

    if(scaleDelta != 0) {
      var i = 0
      while(i < scores.length) {
        scores(i) = java.lang.Math.scalb(scores(i), -scaleDelta)
        i += 1
      }
    }

    currentScale + scaleDelta
  }

  /**
   * Computes the log power of two we'd need to scale by
   * so that the max double is between (2 ** scaleConstant, 2 ** -scaleConstant).
   *
   * @param scores
   * @return
   */
  def computeScaleDelta(scores: Array[Double]):Int = {
    var maxScale = -10000
    var i = 0
    while(i < scores.length) {
      val score = scores(i)
      if(score != 0.0) {
        val exp = java.lang.Math.getExponent(score)
        maxScale = math.max(maxScale, exp)
      }
      i += 1
    }

    // if we scalb by -value, then all doubles will be in the range we want.
    // note that minScale < 0 in general
    if(maxScale == -10000) 0
    else if(maxScale > scaleConstant) scaleConstant * (maxScale / scaleConstant)
    else if(maxScale < -scaleConstant) scaleConstant * (maxScale / scaleConstant)
    else 0
  }

  def determineScale(score: Double, oldScale: Int):Int = {
    if(score != 0.0) {
      val maxScale = java.lang.Math.getExponent(score)
      if(maxScale == -10000) oldScale
      else if(maxScale > scaleConstant) oldScale + scaleConstant * (maxScale / scaleConstant)
      else if(maxScale < -scaleConstant) oldScale + scaleConstant * (maxScale / scaleConstant)
      else oldScale
    } else {
      Int.MinValue
    }
  }

  def scaleArrayToScale(scores: Array[Double], currentScale: Int, targetScale: Int) {
    val scaleDelta = targetScale - currentScale

    if(scaleDelta != 0) {
      var i = 0
      while(i < scores.length) {
        scores(i) = java.lang.Math.scalb(scores(i), -scaleDelta)
        i += 1
      }
    }

  }

  /**
   * Sums `src` into `dest` assuming they're at different scales. `src` may be longer than
   * `dest`, which is useful for allocating a large work buffer.
   * @param src
   * @param srcScale
   * @param dest
   * @param destScale
   * @return the new scale
   */
  def sumArrays(src: Array[Double], srcScale: Int, dest: Array[Double], destScale: Int): Int = {
    if (destScale == srcScale) {
      axpy(1.0, src, dest)
      destScale
    // minValue in dest is 2**(-145+destScale), max in src is 2**(145 + srcScale)
    // if (-145-145) + (destScale-srcScale) > 53
    // then this is a noop.
    } else if (destScale - srcScale > 53 + 2 * scaleConstant) {
      destScale
    } else if (srcScale - destScale > 53 + 2 * scaleConstant) {
      System.arraycopy(src,0,dest,0,dest.length)
      srcScale
    } else if (srcScale > destScale) {
      scaleArrayToScale(dest, destScale, srcScale)
      var i = 0
      while(i < dest.length) {
        dest(i) += src(i)
        i += 1
      }
      srcScale
    } else {
      // hybrid axpy/scale
      val scaleDelta = destScale - srcScale
      var i = 0
      while(i < dest.length) {
        dest(i) += java.lang.Math.scalb(src(i), -scaleDelta)
        i += 1
      }

      destScale
    }
  }

  /**
   * Converts the scaled value into "normal" space
   */
  def unscaleValue(score: Double, currentScale: Int) = {
    java.lang.Math.scalb(score, currentScale)
  }

  /**
   * Converts the scaled value into "normal" space
   */
  def scaleValue(score: Double, currentScale: Int, targetScale: Int) = {
    java.lang.Math.scalb(score, currentScale - targetScale)
  }

  def toLogSpace(score: Double, currentScale: Int) = {
    log(score) + currentScale * log(2d)
  }
}

object Scaling extends Scaling {
  val scaleConstant = 145 // 10^63 more or less
}

