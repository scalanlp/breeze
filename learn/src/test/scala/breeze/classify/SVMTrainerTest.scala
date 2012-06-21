package breeze.classify

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import breeze.linalg._

/**
 * 
 * @author dlwh
 */
//@RunWith(classOf[JUnitRunner])
//class SVMTrainerTest extends ClassifierTrainerTestHarness with ContinuousTestHarness {
//  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] = new Pegasos[L,Counter[T,Double]](200,batchSize=50)
//}

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SMOTrainerTest extends ClassifierTrainerTestHarness with ContinuousTestHarness {
  def trainer[L,T]:Classifier.Trainer[L,Counter[T,Double]] = new SVM.SMOTrainer[L,Counter[T,Double]](10);
}