package breeze.nnet

import breeze.linalg._
import breeze.numerics.sigmoid
import breeze.optimize.BatchDiffFunction
import collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */
class NeuralNetwork(parameters: Array[DenseMatrix[Double]]) extends (DenseVector[Double]=>DenseVector[Double]) {

  def apply(x: DenseVector[Double]) = {
    computeActivations(x).last
  }

  def computeActivations(x: DenseVector[Double]):IndexedSeq[DenseVector[Double]] = {
    val activations = new ArrayBuffer[DenseVector[Double]]
    var activation = x
    activations += x
    for(i <- 0 until parameters.length) {
      val w = parameters(i)
      activation = w * activation
      if(i < parameters.length - 1)
      activation = sigmoid(activation)
      activations += activation
    }
    activations
  }

}

class NNObjective[Output](data: IndexedSeq[(DenseVector[Double],Output)],
                          errorFun: (DenseVector[Double],Output)=>(Double,DenseVector[Double]),
                          layers: Array[Int]) extends BatchDiffFunction[DenseVector[Double]] {


  /**
   * The full size of the data
   */
  def fullRange: IndexedSeq[Int] = 0 until data.length

  def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]): (Double, DenseVector[Double]) = {
    val weights = unrollWeights(x)
    val nn = new NeuralNetwork(weights)
    val data = batch.map(this.data)
    val (loss, gradient) = data.aggregate(null:(Double, Array[DenseMatrix[Double]]))({ (_errors: (Double, Array[DenseMatrix[Double]]), datum: (DenseVector[Double], Output)) =>
      val grad = if(_errors eq null) emptyGradient else _errors._2
      var loss = if(_errors eq null) 0.0 else _errors._1

      // forward pass
      val activations = nn.computeActivations(datum._1)
      // compute task loss and derivatives
      val (taskLoss, taskDerivs) = errorFun(activations.last, datum._2)
      loss += taskLoss

      // back propagate
      var deltas = taskDerivs
      for(layer <- (activations.length-2) to 0 by -1) {
        grad(layer) += deltas * activations(layer).t
        deltas = (weights(layer).t * deltas) :* activations(layer).map(a => a * (1-a))
      }

      loss -> grad
    }, { (a, b) =>
      (a._1 + b._1, for( (aa,bb) <- a._2 zip b._2) yield aa += bb )
    })

    loss -> rollWeights(gradient)
  }

  def extract(x: DenseVector[Double]) = new NeuralNetwork(unrollWeights(x))

  private val weightOffsets = {
    // just an unfold
    val arr = new Array[Int](layers.length)
    var i = 0
    var off = 0
    while(i < (arr.length-1)) {
      // rows x cols
      val size = layers(i) * layers(i + 1)
      arr(i) = off
      off += size
      i += 1
    }
    arr(i) = off
    arr
  }

  private def weightSize = weightOffsets.last

  private def unrollWeights(weights: DenseVector[Double]): Array[DenseMatrix[Double]] = {
    Array.tabulate(layers.length-1){ i => new DenseMatrix(layers(i+1), layers(i), weights.data, weightOffsets(i))}
  }

  private def rollWeights(weights: Array[DenseMatrix[Double]]) = {
    val out = DenseVector.zeros[Double](weightSize)
    for(layer <- 0 until (layers.length-1))
      out(weightOffsets(layer) until weightOffsets(layer + 1)) := new DenseVector(weights(layer).data,weightOffsets(layer), stride = 1, length=layers(layer+1)*layers(layer))
    out
  }

  private def emptyGradient = unrollWeights(DenseVector.zeros(weightSize))

  def initialWeightVector: DenseVector[Double] = {
    val v = DenseVector.rand(weightSize)
    v *= 2.0
    v -= 1.0
  }
}

