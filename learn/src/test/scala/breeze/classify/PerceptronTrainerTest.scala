package breeze.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.classify.SVM.Pegasos
import breeze.linalg._

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class PerceptronTrainerTest extends ClassifierTrainerTestHarness with ContinuousTestHarness {
  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] = new Perceptron.Trainer[L,Counter[T,Double]];
}