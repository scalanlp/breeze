package scalanlp.classify

import scalala.tensor.Counter
import org.scalatest.FunSuite
import scalanlp.data.Example

/**
 * 
 * @author dlwh
 */

trait ClassifierTrainerTestHarness extends FunSuite {
  def trainer[L]: Classifier.Trainer[L,Counter[String,Double]];

  test("simple example") {
    val trainingData = Array (
      Example("cat",Counter.count(Array("fuzzy","claws","small")).mapValues(_.toDouble)),
      Example("bear",Counter.count(Array("fuzzy","claws","big")).mapValues(_.toDouble)),
      Example("cat",Counter.count(Array("claws","medium")).mapValues(_.toDouble))
    )
    val testData = Array(
      Example("cat", Counter.count(Array("claws","small")).mapValues(_.toDouble))
    )

    val r = trainer[String].train(trainingData).classify(testData(0).features)
    assert(r == testData(0).label);
  }

}