package breeze.classify

import breeze.util.Index
import breeze.linalg.DenseMatrix
import de.bwaldvogel.liblinear._

/**
 * A simple wrapper to Liblinear.
 * 
 * @author jasonbaldridge
 */
class LiblinearClassifier(model: Model) {
  lazy val weights = model.getFeatureWeights
  lazy val numClasses = model.getNrClass

  import LiblinearUtil.createNodes

  def apply(nodes: Seq[FeatureNode]) = predict(nodes)

  def predict(nodes: Seq[FeatureNode]) = 
    Linear.predict(model, nodes.toArray)

  def predictDense(inputValues: Seq[Double]) = 
    apply(createNodes(inputValues))

  def predictValues(nodes: Seq[FeatureNode]) = {
    val linearPredictor = Array.fill(numClasses)(0.0)
    Linear.predictValues(model, nodes.toArray, linearPredictor)
    linearPredictor.toSeq
  }

  /**
   * This is an alternative to predictValues that assumes binary classification
   * and assumes that the model won't be asked to use an out-of-bounds feature,
   * both of which cut down on overhead. It will give out-of-bounds exceptions
   * if not used appropriately, and will not work at all for polytomous models.
   */
  def predictValuesBinary(nodes: Seq[FeatureNode]) = {
    var sum = 0.0
    var index = 0
    while(index < nodes.length) {
      val f = nodes(index)
      sum += weights(f.index-1)*f.value
      index += 1
    }
    sum
  }

  /**
   * This is how we'd prefer to write predictValuesBinary, but unfortunately,
   * the while loop is faster than the fold (by about a factor of two, it
   * seems). :(
   */
  def predictValuesBinarySlow(nodes: Seq[FeatureNode]) =
    nodes.foldLeft(0.0)((accum, f) => accum + weights(f.index-1)*f.value)

  def predictValuesDense(inputValues: Seq[Double]) = 
    predictValues(createNodes(inputValues))

}

/**
 * Companion object to train a Liblinear classifier from data.
 * 
 * @author jasonbaldridge
 */
class LiblinearTrainer(
  regularization: Double = 1.0, 
  eps: Double = 0.01,
  showDebug: Boolean = false
) {

  import LiblinearUtil._

  if (!showDebug) Linear.disableDebugOutput

  /** 
   * Takes input with counts of successes and failures to produce individual
   * binary classification training instances with replication of features.
   * (Based on the equivalence of binomial-logistic and binary-logistic models
   * noted in Gelman and Hill (2007) p. 117, last paragraph.)
   */
  def applyBinomialExpanded(
    binomialResponses: Seq[(Int,Int)],
    designMatrix: Seq[Array[FeatureNode]],
    numFeatures: Int
  ) = {
    val (responses, expandedDesignMatrix) = 
      binomialResponses.zip(designMatrix).flatMap { 
        case((countSuccess, countFail), features) =>
          ((1 to countSuccess).map(i => (0, features))
           ++ (1 to countFail).map(i => (1, features)))
      }.unzip

    // Add one to number of features because of the +1 mapping in createNodes.
    apply(responses.toArray, expandedDesignMatrix.toArray, numFeatures+1)
  }


  def applyDense(
    responses: Seq[Int],
    designMatrix: DenseMatrix[Double]
  ) =
    apply(responses.toArray, createLiblinearMatrix(designMatrix), designMatrix.cols)

  def applyIndexed(
    responses: Seq[Int],
    designMatrix: Seq[Seq[(Int,Double)]], 
    numFeatures: Int
  ) =
    apply(responses.toArray, createLiblinearMatrix(designMatrix), numFeatures)

  def apply(
    responses: Array[Int],
    designMatrix: Array[Array[FeatureNode]],
    numFeatures: Int
  ) = {

    val problem = new Problem
    problem.y = responses.toArray
    problem.x = designMatrix
    problem.l = responses.length
    problem.n = numFeatures
 
    // Can make the solver type a parameter if want to use other solvers in LibLinear.
    val param = new Parameter(SolverType.L2R_LR, regularization, eps)
    val model = Linear.train(problem, param)
    new LiblinearClassifier(model)
  }

}
/**
 * Some convenience methods.
 * 
 * @author jasonbaldridge
 */
object LiblinearUtil {

  def createLiblinearMatrix(designMatrix: Seq[Seq[(Int,Double)]]) =  
    designMatrix.map { features =>
      features.map{ case(a,v) => new FeatureNode(a,v) }.toArray
    }.toArray

  def createLiblinearMatrix(designMatrix: DenseMatrix[Double]) = 
    (0 until designMatrix.rows).map { i => 
      createNodes((0 until designMatrix.cols).map(j => designMatrix(i,j)))
    }.toArray

  /**
   * Converts a 0-based matrix row into an Array of 1-based features for
   * Liblinear.
   */
  def createNodes(inputValues: Seq[Double]) = 
    inputValues
      .zipWithIndex
      .map { case(v, i) => new FeatureNode(i+1,v) }
      .toArray
  
}

/**
 * This is an example app for creating a Liblinear classifier from data that is 
 * stored as string valued features and string valued labels, e.g.
 * 
 * verb=join,noun=board,prep=as,prep_obj=director,V
 * verb=isIs,noun=chairman,prep=of,prep_obj=N.V.,N
 * verb=named,noun=director,prep=of,prep_obj=conglomerate,N
 *
 * These are examples from Ratnarparkhi's classic prepositional phrase attachment
 * dataset, discussed in the following homework:
 *
 *   http://ata-s12.utcompling.com/assignments/classification
 *
 * The homework includes pointers to the data and to Scala code for generating
 * said features.
 *
 * This example handles creating a feature index and getting the examples into the
 * right data structures for training with the logistic regression classifier,
 * which should serve as a useful example for creating features and classifiers
 * using the API.
 * 
 * @author jasonbaldridge
 */ 
object LiblinearClassifierFromCsv {

  import breeze.data._
  import breeze.config._
  import java.io._

  case class Params(
    @Help(text="Input training file in CSV format.") train: File,
    @Help(text="Input eval file in CSV format.") eval: File,
    @Help(text="Regularization value (default 1.0).") reg: Double = 1.0,
    @Help(text="Prints this") help:Boolean = false
  )

  def main(args: Array[String]) {

    val config = CommandLineParser.parseArguments(args)._1
    val params = config.readIn[Params]("")

    // Feature map
    val lmap = Index[String]()
    val fmap = Index[String]()
    fmap.index("DUMMY FEATURE SO THAT LIBLINEAR CAN START WITH 1-BASED INDEX")

    // Read in the training data and index it.
    val trainingData = 
      SparseCsvDataset(io.Source.fromFile(params.train))
        .map(ex => ex.map(_.map(fmap.index(_))).relabel(lmap.index(_)))
        .toList // Need to consume the lines in order to populate the feature map

    val interceptFeature = (fmap.index("intercept"),1.0)

    val (responses, designMatrix) =
        trainingData
          .map(ex => (ex.label, makeLibsvmFeatureVector(ex) ++ List(interceptFeature)))
          .unzip

    // Train the classifier
    val classifier = 
      new LiblinearTrainer(params.reg).applyIndexed(responses, designMatrix, fmap.size)

    // Read in the evaluation data
    val evalData = 
      SparseCsvDataset(io.Source.fromFile(params.eval))
        .map(ex => ex.map(_.flatMap(fmap.indexOpt(_))).relabel(lmap.index(_)))
        .toList
        .map(ex => (ex.label, makeLibsvmFeatureVector(ex) ++ List(interceptFeature)))

    // Get the predictions
    val compare = evalData.map { case (label, features) =>
      (label, classifier(features.map{ case(a,v) => new FeatureNode(a,v) }))
    }

    val correct = compare.count{ case(t,p) => t == p }
    println("Accuracy: " + correct/compare.length.toDouble*100)

  }

  /**
   * Creates a seq of libsvm format features from the example. Assumes we have
   * observations of each feature, and these could show up multiple times, so
   * we count those occurences and use those as the values stored in the
   * vector.
   */
  private def makeLibsvmFeatureVector (example: Example[Int, Seq[Int]]) = {
    example
      .features
      .groupBy(x=>x)
      .mapValues(_.length.toDouble)
      .toList
      .sorted
  }
  
  /**
   * Read in a dataset and create Examples from it. Don't do any feature indexation,
   * since for training data we want to build the index, but for eval data we just
   * want to use it.
   */
  object SparseCsvDataset {
  
    def apply(dataSource: io.Source): Iterator[Example[String, Seq[String]]] =
      dataSource
        .getLines
        .zipWithIndex
        .map { case(line,rowId) => { 
          val lineData = line.split(",")
          val (features, label) = (lineData.dropRight(1), lineData.last);
          Example[String, Seq[String]](label=label, features=features, id=rowId.toString)
        }}

  }

}
