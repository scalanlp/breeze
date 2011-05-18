package scalanlp.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalala.tensor.mutable.Counter
import scalanlp.classify.SVM.Pegasos
/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class PerceptronTrainerTest extends ClassifierTrainerTestHarness with ContinuousTestHarness {
  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] = new Perceptron.Trainer[L,Counter[T,Double]];
}