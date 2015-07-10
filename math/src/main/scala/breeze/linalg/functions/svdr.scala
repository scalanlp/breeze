package breeze.linalg.functions


import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.svd.{DenseSVD, SVD}
import breeze.numerics.{abs, signum}
import breeze.stats.distributions.Rand


/**
 * Approximate truncated randomized SVD
 */
object svdr extends UFunc {

  implicit object SvdR_DM_Impl2 extends Impl2[DenseMatrix[Double], Int, DenseSVD] {
    def apply(M: DenseMatrix[Double], k: Int): DenseSVD =
      doSVDR_Double(M, k, nOversamples = 10, nIter = 0)
  }

  implicit object SvdR_DM_Impl3 extends Impl3[DenseMatrix[Double], Int, Int, DenseSVD] {
    def apply(M: DenseMatrix[Double], k: Int, nOversamples: Int): DenseSVD =
      doSVDR_Double(M, k, nOversamples, nIter = 0)
  }

  implicit object SvdR_DM_Impl4 extends Impl4[DenseMatrix[Double], Int, Int, Int, DenseSVD] {
    def apply(M: DenseMatrix[Double], k: Int, nOversamples: Int, nIter: Int): DenseSVD =
      doSVDR_Double(M, k, nOversamples, nIter)
  }

  /**
   * Computes an approximate truncated randomized SVD. Fast on large matrices with limited
   * number of singular values and vectors that are necessary to extract
   *
   * @param M Matrix to decompose
   * @param k Number of singular values and vectors to extract
   * @param nOversamples Additional number of random vectors to sample the range of M so as
   *                     to ensure proper conditioning. The total number of random vectors
   *                     used to find the range of M is [k + nOversamples]
   * @param nIter Number of power iterations (can be used to deal with very noisy problems)
   * @return The singular value decomposition (SVD) with the singular values,
   *         the left and right singular vectors
   *
   * ==References==
   *
   * Finding structure with randomness: Stochastic algorithms for constructing
   * approximate matrix decompositions
   * Halko, et al., 2009 [[http://arxiv.org/abs/arXiv:0909.4061]]
   */
  private def doSVDR_Double(M: DenseMatrix[Double],
                            k: Int,
                            nOversamples: Int = 10,
                            nIter: Int = 0): DenseSVD = {

    require(k <= (M.rows min M.cols), "Number of singular values should be less than min(M.rows, M.cols)")

    val nRandom = k + nOversamples

    val Q = randomizedStateFinder(M, nRandom, nIter)

    val b = Q.t * M

    val SVD(w2, _s, _v) = svd.reduced(b)

    val _u = Q * w2

    val (u, v) = flipSVDSigns(_u, _v)

    SVD(u(::, 0 until k), _s(0 until k), v(0 until k, ::))
  }

  /**
   * Computes an orthonormal matrix whose range approximates the range of M
   *
   * @param M The input data matrix
   * @param size Size of the matrix to return
   * @param nIter Number of power iterations used to stabilize the result
   * @return A size-by-size projection matrix Q
   *
   * ==Notes==
   *
   * Algorithm 4.3 of "Finding structure with randomness:
   * Stochastic algorithms for constructing approximate matrix decompositions"
   * Halko, et al., 2009 (arXiv:909) [[http://arxiv.org/pdf/0909.4061]]
   */
  private def randomizedStateFinder(M: DenseMatrix[Double],
                                    size: Int,
                                    nIter: Int): DenseMatrix[Double] = {
    val R = DenseMatrix.rand(M.cols, size, rand = Rand.gaussian)
    val Y = M * R
    for (a <- 0 until nIter) Y := M * (M.t * Y)
    val q = qr.reduced.justQ(Y)
    q
  }

  /**
   * Resolves the sign ambiguity. Largest in absolute value entries of u columns are always positive
   *
   * @param u left singular vectors
   * @param v right singular vectors
   * @return left and right singular vectors with resolved sign ambiguity
   */
  private def flipSVDSigns(u: DenseMatrix[Double],
                           v: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = {
    import DenseMatrix.canMapValues
    val abs_u = abs(u)
    val max_abs_cols = (0 until u.cols).map(c => argmax(abs_u(::, c)))
    val signs = max_abs_cols.zipWithIndex.map(e => signum(u(e._1, e._2)))
    signs.zipWithIndex.foreach(s => {
      u(::, s._2) :*= s._1
      v(s._2, ::) :*= s._1
    })
    (u, v)
  }
}
