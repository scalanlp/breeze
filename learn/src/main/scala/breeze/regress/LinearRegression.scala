package breeze.regress

import breeze.linalg.{DenseVector, DenseMatrix}


/**
 * 
 * @author dlwh
 */
object LinearRegression {
  /**
   * Given a set of data points as rows in a matrix and their corresponding outputs, produces a vector of weights
   * s.t. output(i) \approx observations(i) dot weights
   */
  def regress(observations: DenseMatrix[Double], outputs: DenseVector[Double]):DenseVector[Double] = {
    val cov =  (DenseMatrix.zeros[Double](observations.cols,observations.cols) + (observations.t * observations))
    val scaled = DenseVector.zeros[Double](observations.cols) + (observations.t * outputs)
    cov \ scaled
  }

}