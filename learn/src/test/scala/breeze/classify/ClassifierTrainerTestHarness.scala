package breeze.classify

import org.scalatest.FunSuite
import breeze.data.{DataMatrix, Example}
import breeze.stats.ContingencyStats
import breeze.linalg._

/**
 * 
 * @author dlwh
 */

trait ClassifierTrainerTestHarness extends FunSuite {
  def trainer[L,F]: Classifier.Trainer[L,Counter[F,Double]]

  test("simple example") {
    val trainingData = Array (
      Example("cat",Counter.count("fuzzy","claws","small").mapValues(_.toDouble)),
      Example("bear",Counter.count("fuzzy","claws","big").mapValues(_.toDouble)),
      Example("cat",Counter.count("claws","medium").mapValues(_.toDouble))
    )
    val testData = Array(
      Example("cat", Counter.count("claws","small").mapValues(_.toDouble))
    )

    val r = trainer[String,String].train(trainingData).classify(testData(0).features)
    assert(r == testData(0).label)
  }





}

trait ContinuousTestHarness extends ClassifierTrainerTestHarness {
  test("prml") {
    val classifier = trainer[Int,Int].train(PRMLData.classification)
    val contingencyStats = ContingencyStats(classifier, PRMLData.classification)
    println(contingencyStats)
    assert(contingencyStats.microaveraged.precision > 0.65,ContingencyStats(classifier,PRMLData.classification))
  }
}

// Data from Bishop
object PRMLData {
  val classification = {
    val url = PRMLData.getClass().getClassLoader().getResource("breeze/classify/data/prml")
    val datamatrix = DataMatrix.fromURL(url,3)
    datamatrix.rows.map { ex =>
      ex.map{row =>
        val r = Counter[Int,Double]()
        for( (v,k) <- row.zipWithIndex) {
          r(k) = v
        }
        r
      }.relabel(_.toInt)
    }
  }
}