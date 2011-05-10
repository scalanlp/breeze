package scalanlp.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scalala.tensor.mutable.Counter
import scalanlp.classify.SVM.Pegasos
import scalanlp.util.ConsoleLogging

/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SVMTrainerTest extends ClassifierTrainerTestHarness {
  def trainer[L]:Classifier.Trainer[L,Counter[String,Double]] = new Pegasos[L,Counter[String,Double]](10,batchSize=1) with ConsoleLogging;
}