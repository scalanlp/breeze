package breeze.optimize

import breeze.linalg.DenseVector
import org.scalatest.FunSuite

class FirstOrderMinimizerTest extends FunSuite {

  test("default relative gradient convergence check for negative function values") {
    val value = -10
    val gradient = DenseVector.fill(5){1E-7}
    val convergenceCheck = FirstOrderMinimizer.defaultConvergenceCheck[DenseVector[Double]](100, 1E-5, relative = true)
    val state = FirstOrderMinimizer.State[DenseVector[Double], Any, Any](
      DenseVector.ones(5),
      value,
      gradient,
      value,
      gradient,
      8,
      -1,
      LBFGS.ApproximateInverseHessian[DenseVector[Double]](4),
      ()
    )
    val checkResult = convergenceCheck.apply(state, convergenceCheck.initialInfo)
    assert(checkResult.contains(FirstOrderMinimizer.GradientConverged))
  }

}
