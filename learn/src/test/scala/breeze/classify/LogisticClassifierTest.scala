package breeze.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalala.tensor.mutable.Counter
import breeze.optimize.FirstOrderMinimizer.OptParams

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class LogisticClassifierTest extends ClassifierTrainerTestHarness with ContinuousTestHarness {
  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] = new LogisticClassifier.Trainer[L,Counter[T,Double]](OptParams(useStochastic=true,tolerance=1E-3))
}
