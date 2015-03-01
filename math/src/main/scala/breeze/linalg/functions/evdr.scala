package breeze.linalg.functions


import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.eig.Eig
import breeze.linalg.eigSym.{DenseEigSym, EigSym}
import breeze.numerics._
import breeze.stats.distributions.Rand


/**
 * Approximate truncated randomized EVD
 */
object evdr extends UFunc {

  implicit object EVDR_DM_Impl2 extends Impl2[DenseMatrix[Double], Int, DenseEigSym] {
    def apply(M: DenseMatrix[Double], s: Int): DenseEigSym =
      doEigSymDouble(M, s, nOversamples = 10, nIter = 0)
  }

  implicit object EVDR_DM_Impl3 extends Impl3[DenseMatrix[Double], Int, Int, DenseEigSym] {
    def apply(M: DenseMatrix[Double], s: Int, nOversamples: Int): DenseEigSym =
      doEigSymDouble(M, s, nOversamples, nIter = 0)
  }

  implicit object EVDR_DM_Impl4 extends Impl4[DenseMatrix[Double], Int, Int, Int, DenseEigSym] {
    def apply(M: DenseMatrix[Double], s: Int, nOversamples: Int, nIter: Int): DenseEigSym =
      doEigSymDouble(M, s, nOversamples, nIter)
  }

  /**
   * Computes an approximate truncated randomized EVD. Fast on large matrices.
   *
   * @param M Matrix to decompose
   * @param s Number of columns in orthonormal matrix (sketch size)
   * @param nOversamples Additional number of random vectors to sample the range of M so as
   *                     to ensure proper conditioning. The total number of random vectors
   *                     used to find the range of M is [s + nOversamples]
   * @param nIter Number of power iterations (can be used to deal with very noisy problems)
   * @return The eigenvalue decomposition (EVD) with the eigenvalues and the eigenvectors
   *
   * ==References==
   *
   * Finding structure with randomness: Stochastic algorithms for constructing
   * approximate matrix decompositions
   * Halko, et al., 2009 [[http://arxiv.org/abs/arXiv:0909.4061]]
   */
  private def doEigSymDouble(M: DenseMatrix[Double],
                             s: Int,
                             nOversamples: Int = 10,
                             nIter: Int = 0): DenseEigSym = {

    require(s <= (M.rows min M.cols), "Number of columns in orthonormal matrix should be less than min(M.rows, M.cols)")
    require(s >= 1, "Sketch size should be greater than 1")

    val nRandom = s + nOversamples

    val Q = randomizedStateFinder(M, nRandom, nIter)

    val b = Q.t * (M * Q)

    val Eig(w, _, v) = eig(b)

    val _u = Q * v

    val u = flipSigns(_u)

    EigSym(w, u)
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
   * @param u eigenvectors
   * @return eigenvectors with resolved sign ambiguity
   */
  private def flipSigns(u: DenseMatrix[Double]): DenseMatrix[Double] = {
    val abs_u = abs(u)
    val max_abs_cols = (0 until u.cols).map(c => argmax(abs_u(::, c)))
    val signs = max_abs_cols.zipWithIndex.map(e => signum(u(e._1, e._2)))
    signs.zipWithIndex.foreach(s => {
      u(::, s._2) :*= s._1
    })
    u
  }
}

