package breeze.optimize

import breeze.optimize.FirstOrderMinimizer.OptParams

/**
 *
 * @author dlwh
 */
sealed trait OptimizationOption extends (OptParams => OptParams)

object OptimizationOption {
  implicit def fromOptParams(optParams: OptParams): OptimizationOption =
    new OptimizationOption {
      def apply(v1: OptParams): OptParams = optParams
    }

}

case class MaxIterations(num: Int) extends OptimizationOption {
  def apply(params: OptParams): OptParams = {
    params.copy(maxIterations = num)
  }
}

case class L2Regularization(value: Double = 1.0) extends OptimizationOption {
  def apply(params: OptParams): OptParams = {
    params.copy(useL1 = false, regularization = value)
  }

}

case class L1Regularization(value: Double = 1.0) extends OptimizationOption {
  def apply(params: OptParams): OptParams = {
    params.copy(useL1 = true, regularization = value)
  }

}

case class BatchSize(size: Int) extends OptimizationOption {
  def apply(params: OptParams): OptParams = {
    params.copy(batchSize = size)
  }
}

case class StepSizeScale(alpha: Double = 1.0) extends OptimizationOption {
  def apply(params: OptParams): OptParams = {
    params.copy(alpha = alpha)
  }
}

case class Tolerance(fvalTolerance: Double = 1e-5, gvalTolerance: Double = 1e-6) extends OptimizationOption {
  def apply(params: OptParams): OptParams = {
    // TODO: gvaltolerance
    params.copy(tolerance = fvalTolerance)
  }
}

case object PreferOnline extends OptimizationOption {
  def apply(params: OptParams): OptParams = {
    params.copy(useStochastic = true)
  }
}

case object PreferBatch extends OptimizationOption {
  def apply(params: OptParams): OptParams = {
    params.copy(useStochastic = false)
  }
}
