package breeze.classify

import breeze.nnet.{NNObjective, NeuralNetwork}
import breeze.util.{Encoder, Index}
import breeze.linalg._
import chalk.data.Example
import breeze.numerics._
import breeze.optimize.FirstOrderMinimizer.OptParams

/**
 * A NeuralNetwork classifier uses a neural network to get unnormalize log probabilities
 * for the scores of the classifier. These are used to predict terms.
 * @author dlwh
 */
class NNetClassifier[L, T](nnet: NeuralNetwork,
                           inputEncoder: T=>DenseVector[Double],
                           labelIndex: Index[L]) extends Classifier[L, T] {
  /**For the observation, return the score for each label that has a nonzero
   * score.
   */
  def scores(o: T): Counter[L, Double] = {
    Encoder.fromIndex(labelIndex).decode(nnet(inputEncoder(o)))
  }
}

object NNetClassifier {
  class CounterTrainer[L, T](opt: OptParams = OptParams(),layersIn:Array[Int] = Array(100)) extends Classifier.Trainer[L, Counter[T, Double]] {
    type MyClassifier = NNetClassifier[L, Counter[T, Double]]

    def train(data: Iterable[Example[L, Counter[T, Double]]]) = {
      val labels = Index[L]()
      data foreach { labels index _.label}
      val featureIndex = Index[T]()
      for(d <- data; f <- d.features.keysIterator) featureIndex.index(f)
      val fEncoder = Encoder.fromIndex(featureIndex)
      val processedData = data.toArray.par.map { d =>
        fEncoder.encodeDense(d.features) -> labels(d.label)
      }
      // log loss error function log(  input(gold)/(sum of all outputs))
      def errorFun(input: DenseVector[Double], label: Int) = {
        val sm = softmax(input)
        val obj = sm -  input(label)
        val deriv = exp(input - sm)
        deriv(label) -= 1
        obj -> deriv
      }
      val layers = Array(featureIndex.size) ++ layersIn ++ Array(labels.size)
      val obj = new NNObjective(processedData.toIndexedSeq, errorFun, layers)
      val guess = obj.initialWeightVector
      val weights = opt.minimize(obj,guess)
      new NNetClassifier(obj.extract(weights), {fEncoder.encodeDense(_:Counter[T, Double], true)}, labels)
    }
  }
}
