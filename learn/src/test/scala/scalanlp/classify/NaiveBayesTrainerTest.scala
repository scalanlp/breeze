package scalanlp.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalala.tensor.mutable.Counter

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class NaiveBayesTrainerTest extends ClassifierTrainerTestHarness {
  def trainer[L]:Classifier.Trainer[L,Counter[String,Double]] = new NaiveBayes.Trainer[L,String]();
}