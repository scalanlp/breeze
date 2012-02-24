package scalanlp.sequences

import scalanlp.optimize.FirstOrderMinimizer
import scalanlp.corpora.CONLLSequenceReader
import collection.mutable.ArrayBuffer
import scalanlp.util._
import scalanlp.config.CommandLineParser
import java.io.{InputStream, FileInputStream, File}

/**
 * Runs the CRF and evaluates per-word accuracy. This won't give the right results for NER. Use NERTest for that.
 * @author dlwh
 */
object CRFEvaluate extends App {
  case class Params(input: File, test: File)

  val config = CommandLineParser.parseArguments(args)._1
  val params = config.readIn[Params]("")
  scalanlp.util.logging.ConfiguredLogging.configuration = config

  val crfP = readObject[CRFPackage](params.input)
  import crfP._

  val test= CONLLSequenceReader.readTrain(new FileInputStream(params.test), params.test.getName).toIndexedSeq

  val testProcessed = for(ex <- test) yield ex.map { features =>
    val mapped = IndexedSeq.tabulate(features.length)(i => template.extract(ex, i).map(statIndex(_)).filter(_ != -1).toIndexedSeq)
    mapped -> features.length
  }

  var numRight = 0
  var numGuess = 0
  var numGold = 0
  for( (t,ex) <- (testProcessed zip test)) {
    val guess = crf.viterbi(t.features._1,t.features._2)
    println(t.label,guess,ex.features.map(_(0)))
    val goldEntities = t.label.zipWithIndex.toSet
    val guessEntities = t.label.zipWithIndex.toSet
    val inter = goldEntities.toSet & guessEntities.toSet
    numRight += inter.size
    numGuess += guessEntities.size
    numGold += goldEntities.size
    val localPrec = inter.size * 1.0  / guessEntities.size
    val localRecall = inter.size * 1.0  / goldEntities.size
    val localF1 = 2 * localPrec * localRecall / (localPrec + localRecall)
    println("Local: P %.2f R %.2f F %.2f".format(localPrec,localRecall,localF1))
  }
  val localPrec = numRight  * 1.0 / numGuess
  val localRecall = numRight  * 1.0 / numGold
  val localF1 = 2 * localPrec * localRecall / (localPrec + localRecall)
  println("Total: P %.2f R %.2f F %.2f".format(localPrec,localRecall,localF1))
}

/**
 * Just tags sequences with their labels
 */
object CRFPredict extends App {
  case class Params(input: File, test: String = "STDIN")

  val config = CommandLineParser.parseArguments(args)._1
  val params = config.readIn[Params]("")
  scalanlp.util.logging.ConfiguredLogging.configuration = config

  val crfP = readObject[CRFPackage](params.input)
  import crfP._

  val test =  {
    val stream: InputStream = if(params.test == "STDIN") System.in else new FileInputStream(params.test);
    CONLLSequenceReader.readTrain(stream, params.test).toIndexedSeq
  }

  val testProcessed = for(ex <- test) yield ex.map { features =>
    val mapped = IndexedSeq.tabulate(features.length)(i => template.extract(ex, i).map(statIndex(_)).filter(_ != -1).toIndexedSeq)
    mapped -> features.length
  }

  for( (t,ex) <- (testProcessed zip test)) {
    val guess = crf.viterbi(t.features._1,t.features._2)
    for( (w,l) <- ex.features zip guess) {
      println(w.mkString("\t") +"\t" + l)
    }
    println()
    println()
  }

}


/**
 * Evaluates a sequence model trained with BIO-style tags on whole entity accuracy.
 * Will work for any BIO-task
 */
object NERTest extends App {
  import CRFEvaluate.Params

  val config = CommandLineParser.parseArguments(args)._1
  val params = config.readIn[Params]("")
  scalanlp.util.logging.ConfiguredLogging.configuration = config

  val crfP = readObject[CRFPackage](params.input)
  import crfP._

  val test= CONLLSequenceReader.readTrain(new FileInputStream(params.test), params.test.getName).toIndexedSeq

  val testProcessed = for(ex <- test) yield ex.map { features =>
    val mapped = IndexedSeq.tabulate(features.length)(i => template.extract(ex, i).map(statIndex(_)).filter(_ != -1).toIndexedSeq)
    mapped -> features.length
  }

  def extractNamedEntities(seq: IndexedSeq[String]) = {
    val result = new ArrayBuffer[(String,Range)]()
    var lastBegin = -1
    for(end <- 0 until seq.length) {
      val lbl = seq(end)
      if(!lbl.startsWith("I-")) {
        if(lastBegin >= 0) {
          result += (seq(end-1).drop(2) -> Range(lastBegin,end))
        }
        if(lbl.startsWith("B-")) {
          lastBegin = end
        }
        else lastBegin = -1
      }
    }
    if(lastBegin >= 0) {
      result += (seq(seq.length-1).drop(2) -> Range(lastBegin,seq.length))
    }
    result
  }


  var numRight = 0
  var numGuess = 0
  var numGold = 0
  for( (t,ex) <- (testProcessed zip test)) {
    val guess = crf.viterbi(t.features._1,t.features._2)
    println(t.label,guess,ex.features.map(_(0)))
    val goldEntities = extractNamedEntities(t.label)
    val guessEntities = extractNamedEntities(guess)
    val inter = goldEntities.toSet & guessEntities.toSet
    numRight += inter.size
    numGuess += guessEntities.size
    numGold += goldEntities.size
    val localPrec = inter.size * 1.0  / guessEntities.size
    val localRecall = inter.size * 1.0  / goldEntities.size
    val localF1 = 2 * localPrec * localRecall / (localPrec + localRecall)
    println("Local: P %.2f R %.2f F %.2f".format(localPrec,localRecall,localF1))
  }
  val localPrec = numRight  * 1.0 / numGuess
  val localRecall = numRight  * 1.0 / numGold
  val localF1 = 2 * localPrec * localRecall / (localPrec + localRecall)
  println("Total: P %.2f R %.2f F %.2f".format(localPrec,localRecall,localF1))


}