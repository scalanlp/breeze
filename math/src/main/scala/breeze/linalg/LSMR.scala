package breeze.linalg

import breeze.linalg.operators.OpMulMatrix
import breeze.linalg.support.CanTranspose
import breeze.math.MutableInnerProductVectorSpace
import breeze.numerics.{abs, sqrt}
import breeze.util.SerializableLogging

/**
 * Nearly direct port of
 * http://www.mathworks.com/matlabcentral/fileexchange/27183-lsmr--an-iterative-algorithm-for-least-squares-problems
 * (BSD licensed code)
 *
 * http://web.stanford.edu/group/SOL/software/lsmr/
 *
 * The only difference is that they square the regularization factor.
 *
 *
 * @author dlwh
 **/
object LSMR extends SerializableLogging {

  /**
   * Solves the problem min pow(norm(A * x - b), 2) + regularization * pow(norm(x), 2)
   */
  def solve[M, MT, V](
      A: M,
      b: V,
      regularization: Double = 0.0,
      tolerance: Double = 1e-9,
      maxIter: Int = 1000,
      quiet: Boolean = false
  )(implicit
      multMV: OpMulMatrix.Impl2[M, V, V],
      transA: CanTranspose[M, MT],
      multMTV: OpMulMatrix.Impl2[MT, V, V],
      ispace: MutableInnerProductVectorSpace[V, Double]
  ): V = {
    import ispace._

    val lambda = math.sqrt(regularization)

    val At = transA(A)

    def sqr(x: Double) = x * x

    val atol, btol = tolerance

    var u = copy(b)
    val normb = norm(u)
    var beta = norm(u)
    if (beta > 0) {
      u /= beta
    }

    var v = multMTV(At, u)
    var x = v * 0.0
    var alpha = norm(v)
    if (alpha > 0) {
      v /= alpha
    }

    var alphabar = alpha
    var zetabar = alpha * beta
    var rho = 1.0
    var rhobar = 1.0
    var cbar = 1.0
    var sbar = 0.0
    var zeta = 0.0

    var h = v
    var hbar = h * 0.0

    var betadd = beta
    var betad = 0.0
    var rhodold = 1.0
    var thetatilde = 0.0
    var tautildeold = 0.0
    var d = 0.0

    var normA2 = sqr(alpha)
    var maxrbar = 0.0
    var minrbar = 1e100

    var converged = false
    var iter = 0
    while (!converged && iter < maxIter) {
      iter += 1
      u = multMV(A, v) - u * alpha
      beta = norm(u)

      if (beta > 0) {
        u /= beta
        v = multMTV(At, u) - v * beta
      }

      alpha = norm(v)
      if (alpha > 0) v /= alpha

      // Construct rotation Qhat_{k,2k+1}.

      val alphahat = norm(DenseVector(alphabar, lambda))
      val chat = alphabar /? alphahat
      val shat = lambda /? alphahat

      // Use a plane rotation (Q_i) to turn B_i to R_i.

      val rhoold = rho
      rho = norm(DenseVector(alphahat, beta))
      val c = alphahat /? rho
      val s = beta /? rho
      val thetanew = s * alpha
      alphabar = c * alpha

      // Use a plane rotation (Qbar_i) to turn R_i^T to R_i^bar.

      val rhobarold = rhobar
      val zetaold = zeta
      val thetabar = sbar * rho
      val rhotemp = cbar * rho
      rhobar = norm(DenseVector(cbar * rho, thetanew))
      cbar = cbar * rho /? rhobar
      sbar = thetanew /? rhobar
      zeta = cbar * zetabar
      zetabar = -sbar * zetabar

      // Update h, h_hat, x.

      hbar = h - hbar * (thetabar * rho / (rhoold * rhobarold))
      x = x + hbar * (zeta /? (rho * rhobar))
      h = v - h * (thetanew /? rho)

      // Estimate of ||r||.

      // Apply rotation Qhat_{k,2k+1}.
      val betaacute = chat * betadd
      val betacheck = -shat * betadd

      // Apply rotation Q_{k,k+1}.
      val betahat = c * betaacute
      betadd = -s * betaacute

      // Apply rotation Qtilde_{k-1}.
      // betad = betad_{k-1} here.

      val thetatildeold = thetatilde
      val rhotildeold = norm(DenseVector(rhodold, thetabar))
      val ctildeold = rhodold /? rhotildeold
      val stildeold = thetabar /? rhotildeold
      thetatilde = stildeold * rhobar
      rhodold = ctildeold * rhobar
      betad = -stildeold * betad + ctildeold * betahat

      // betad   = betad_k here.
      // rhodold = rhod_k  here.

      tautildeold = (zetaold - thetatildeold * tautildeold) /? rhotildeold
      val taud = (zeta - thetatilde * tautildeold) /? rhodold
      d = d + sqr(betacheck)
      val normr = sqrt(d + sqr(betad - taud) + sqr(betadd))

      // Estimate ||A||.
      normA2 = normA2 + sqr(beta)
      val normA = sqrt(normA2)
      normA2 = normA2 + sqr(alpha)

      // Estimate cond(A).
      maxrbar = max(maxrbar, rhobarold)
      if (iter > 1) {
        minrbar = min(minrbar, rhobarold)
      }

      var condA = max(maxrbar, rhotemp) /? min(minrbar, rhotemp)

      // Estimate cond(A).
      maxrbar = max(maxrbar, rhobarold)
      if (iter > 1) {
        minrbar = min(minrbar, rhobarold)
      }
      condA = max(maxrbar, rhotemp) /? min(minrbar, rhotemp)

      // Test for convergence.

      // Compute norms for convergence testing.
      val normAr = abs(zetabar)
      val normx = norm(x)

      // Now use these norms to estimate certain other quantities,
      // some of which will be small near a solution.

      val test1 = normr /? normb
      val test2 = normAr /? (normA * normr)
      val rtol = btol + atol * normA * normx / normb

      if (!quiet)
        logger.info(
          f"Residual: $normr%.2g $normAr%.2g " +
            f":: convtest1: $test1%.2g <? $rtol%.2g :: convtest2: $test2%.2g <? $atol%.2g"
        )

      converged = normr == 0.0 || (iter >= maxIter) || (test1 < rtol) || (test2 < atol)
    }

    x
  }

  private implicit class SafeDiv(val __x: Double) extends AnyVal {
    def /?(y: Double): Double = if (y == 0) __x else __x / y
  }

}
