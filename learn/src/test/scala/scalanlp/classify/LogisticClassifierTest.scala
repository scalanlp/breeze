package scalanlp.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalala.tensor.mutable.Counter
/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class LogisticClassifierTest extends ClassifierTrainerTestHarness with ContinuousTestHarness {
  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] = new LogisticClassifier.Trainer[L,Counter[T,Double]];
}

