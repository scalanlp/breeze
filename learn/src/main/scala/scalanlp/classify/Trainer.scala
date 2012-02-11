package scalanlp.classify

import scalanlp.config._
import scalanlp.data.SparseFeatureDataset
import io.Source
import scalala.tensor.Counter
import scalanlp.stats.ContingencyStats
import scalala.generic.collection.{CanCopy, CanCreateZerosLike}
import scalala.collection.sparse.SparseArray
import scalala.tensor.sparse.{SparseVectorCol, SparseVector}
import scalanlp.serialization.DataSerialization
import java.io._

case class TrainerParams(
  @Help(text="The kind of classifier to train. {Logistic,SVM,Pegasos}") `type`: String= "Logistic",
  @Help(text="Input file in svm light format.") input: File= new java.io.File("train"),
  @Help(text="Output file for the serialized classifier.") output: File = new File("classifier.ser"),
  @Help(text="Prints this") help:Boolean = false)

/**
 * Class that builds a classifier
 * @author dlwh
 */
object Trainer extends App {
  val config = CommandLineParser.parseArguments(args)._1
  val params = config.readIn[TrainerParams]("");
  if(params.help) {
    println(GenerateHelp[TrainerParams](config))
  } else {
    val input = SparseFeatureDataset.fromSource[Int](Source.fromFile(params.input),params.input.getName)
    type TheClassifier = LinearClassifier[Int,LFMatrix[Int,SparseVector[Double]],Counter[Int,Double],SparseVector[Double]]
    // TODO: put this in Scalala

    val trainer:Classifier.Trainer[Int,SparseVector[Double]] { type MyClassifier = TheClassifier } = params.`type`.toLowerCase match {
      case "logistic" => new LogisticClassifier.Trainer[Int,SparseVector[Double]]
      case "svm" => new SVM.SMOTrainer[Int,SparseVector[Double]]()
      case "pegasos" => new SVM.Pegasos[Int,SparseVector[Double]](30 * input.examples.length)
    }
    val classifier = trainer.train(input.examples)
    println("Saving to " + params.output)
    params.output.getAbsoluteFile.getParentFile.mkdirs()
    val out = new ObjectOutputStream(new FileOutputStream(params.output))
    DataSerialization.write(out, classifier)
    out.close()
    val clss = DataSerialization.read[trainer.MyClassifier](new ObjectInputStream(new FileInputStream(params.output)))
    println("Performance on training set: ")
    println(ContingencyStats(classifier,input.examples))

  }


}