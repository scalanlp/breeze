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
class SVMTrainerTest extends ClassifierTrainerTestHarness with ContinuousTestHarness {
  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] = new Pegasos[L,Counter[T,Double]](200,batchSize=1);
}