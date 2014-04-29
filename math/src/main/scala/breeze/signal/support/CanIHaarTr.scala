package breeze.signal.support

import breeze.linalg.{DenseVector, DenseMatrix}

/**
 * Construction delegate for getting the IFHT of a value of type InputType.
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call this implicit delegate directly.
 *
 * @author ktakagaki
 */
trait CanIHaarTr[InputType, OutputType] {
  def apply(v1: InputType): OutputType
}

/**
 * Construction delegate for getting the inverse FHT of a value of type InputType.
 */
object CanIHaarTr {

  private val nFactor = 1d / Math.sqrt(2d)

  /** Compute the fht on a given double vector.
    */
  implicit val dvDouble1IFHT : CanIHaarTr[DenseVector[Double], DenseVector[Double]] = {
    new CanIHaarTr[DenseVector[Double], DenseVector[Double]] {
      def apply(v: DenseVector[Double]) = {
        def _ifht(v : DenseVector[Double]) : DenseVector[Double] = {
          if (v.length > 1) {
            // we will inverse the upper left first
            val hs = v.length / 2
            v.slice(0, hs) := _ifht(v.slice(0, hs))
            val x = v.slice(0,hs).toArray.zip(v.slice(hs,v.length).toArray)
            DenseVector(x.map(e => List((e._1 + e._2) * nFactor,(e._1 - e._2) * nFactor)).flatten.toArray)
          } else {
            v
          }
        }
        _ifht(v.copy)
      }
    }
  }

  /** Compute the fht on a given double matrix.
    */
  implicit val dmDouble2IFHT : CanIHaarTr[DenseMatrix[Double], DenseMatrix[Double]] = {
    new CanIHaarTr[DenseMatrix[Double], DenseMatrix[Double]] {
      def apply(m: DenseMatrix[Double]) = {
        def _ifht(m: DenseMatrix[Double], limit : Int) : Unit = if (limit > 1) {
          // inverse the upper left first
          val hs = limit / 2
          _ifht(m, hs)
          for (r <- 0 until limit) {
            val rv =  m.t(0 until limit, r).toArray
            val x = rv.slice(0,hs).zip(rv.slice(hs,limit)).toList
            val v = x.map(e => List((e._1 + e._2) * nFactor,(e._1 - e._2) * nFactor)).flatten.toArray
            for (c <- 0 until limit) m(r,c) = v(c)
          }
          for (c <- 0 until limit) {
            val cv = m(0 until limit,c).toArray
            val x = cv.slice(0,hs).zip(cv.slice(hs,limit)).toList
            val v = x.map(e => List((e._1 + e._2) * nFactor,(e._1 - e._2) * nFactor)).flatten.toArray
            for (r <- 0 until limit) m(r,c) = v(r)
          }
        }
        val r = m.copy
        _ifht(r, Math.max(r.cols, r.rows))
        r
      }
    }
  }

}
