package breeze.linalg

import breeze.linalg.svd.SVD
import breeze.stats.mean

/**
 * Perform Principal Components Analysis on input data. Handles scaling of the
 * when computing the covariance matrix. Lazily produces the scores (the
 * translation of the data to their new coordinates on the PC axes.
 *
 * Input is a matrix that has data points as rows. Variable naming and
 * documentation inspired and used directy from the 'princomp' function in R.
 */
class PCA(val x: DenseMatrix[Double], val covmat: DenseMatrix[Double]) {

  /**
   * The number of observations.
   */
  lazy val nobs = x.rows

  /**
   * The means of each column (axis) of the data.
   */
  lazy val center = mean(x, Axis._0).t

  /**
   * Do SVD on the covariance matrix.
   *
   * eigenvalues: The vector of eigenvalues, from ranked from left to right
   *   with respect to how much of the variance is explained by the
   *   respective component.
   *
   * loadings: the matrix of variable loadings (i.e., a matrix whose rows
   *   contain the eigenvectors (note: in R, the eigenvectors are the columns)
   */
  lazy val SVD(_, eigenvalues, loadings) = svd(covmat)

  /**
   * The standard deviations of the principal components.
   */
  lazy val sdev = eigenvalues.map(math.sqrt)

  /**
   * The proportion of variance explained by each principal component.
   */
  lazy val propvar = normalize(eigenvalues)

  /**
   * The cumulative proportion of variance explained by the first n
   * principal components.
   */
  lazy val cumuvar = propvar.map {
    var c = 0.0;
    d => { c += d; c }
  }

  /**
   * Translate the original data points to the PC axes.
   */
  lazy val scores: DenseMatrix[Double] = (loadings * (x(*, ::) - center).t).t

}
