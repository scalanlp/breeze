package breeze.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg._

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class NaiveBayesTrainerTest extends ClassifierTrainerTestHarness {
  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] = new NaiveBayes.Trainer[L,T]();
}