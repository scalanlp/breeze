package breeze.sequences

import breeze.optimize.FirstOrderMinimizer
import breeze.config.CommandLineParser
import breeze.corpora.CONLLSequenceReader
import java.io.{FileInputStream, File}
import breeze.util.Index
import breeze.sequences.CRF.Featurizer
import collection.mutable.ArrayBuffer
import breeze.text.tokenize.{EnglishWordClassGenerator, WordShapeGenerator}
import scala.collection.IndexedSeq
import io.Source
import breeze.data.Observation

/**
 * This class is just for holding onto all the information produced by CRFTrain.
 *
 * CRFTest can process it.
 *
 * @param crf the crf
 * @param template the feature template created using this CRF
 * @param statIndex the index for features (features without labels)
 */
case class CRFPackage(crf: CRF[String,Seq[Seq[Int]]],
                    template: FeatureTemplate,
                    statIndex: Index[SuffStat]) {

  def labelIndex = crf.transitions.index

  def calibrate(ex: Observation[IndexedSeq[IndexedSeq[String]]]) = {
    val processed = process(ex)
    crf.calibrate(processed._1,processed._2)
  }

  def viterbi(ex: Observation[IndexedSeq[IndexedSeq[String]]]) = {
    val processed = process(ex)
    crf.viterbi(processed._1,processed._2)
  }

  def process(ex: Observation[IndexedSeq[IndexedSeq[String]]]) = {
    val mapped = IndexedSeq.tabulate(ex.features.length)(i => template.extract(ex, i).map(statIndex(_)).filter(_ != -1).toIndexedSeq)
    mapped -> ex.features.length
  }
}

/**
 *
 * Reads in CoNLL-style input files (space separated columns of features, one row per word (blank line to delimit sentences).
 * The last column is the output label.
 *
 * Builds a sequence model over those labels.
 *
 * @author dlwh
 */
object CRFTrain extends App {
  case class Params(opt: FirstOrderMinimizer.OptParams,
                    train: File,
                    output: File = null,
                    test: File = null,
                    template: File = null,
                    testHasGold: Boolean = true,
                    startSymbol: String = "START")

  val config = CommandLineParser.parseArguments(args)._1
  val params = config.readIn[Params]("")
  breeze.util.logging.ConfiguredLogging.configuration = config
  import params.startSymbol
  val train = CONLLSequenceReader.readTrain(new FileInputStream(params.train), params.train.getName).toIndexedSeq
  val labelIndex = Index(Iterator(startSymbol) ++ {for(ex <- train.iterator; l <- ex.label) yield l})

  case class Feature(stat: SuffStat, lbl: Int) extends CRF.Feature

  val template: FeatureTemplate = if(params.template eq null) {

    // default good set of features. Assumes the first column is the word, and the second a pos-tag. Works well for NER.
    val columns = (0 until train(0).features.apply(0).length) map(ColumnTemplate(_,0))
    val backone = (0 until train(0).features.apply(0).length) map(ColumnTemplate(_,-1))
    val nextone = (0 until train(0).features.apply(0).length) map(ColumnTemplate(_,1))
    val words = Seq(ColumnTemplate(0,-2),ColumnTemplate(0,2),
      NgramTemplate(0,0),
      NgramTemplate(0,offset= -1),
      NgramTemplate(0,offset= 1),
      ProductTemplate(ColumnTemplate(0,-1),ColumnTemplate(0,1)),
      ProductTemplate(ColumnTemplate(0,-1),ColumnTemplate(0,0),ColumnTemplate(0,1)),
      ProductTemplate(ColumnTemplate(0,0),ColumnTemplate(0,1)),
      ProductTemplate(ColumnTemplate(1,-1),ColumnTemplate(1,1)),
      ProductTemplate(ColumnTemplate(1,0),ColumnTemplate(1,1))
    )
    val wordShape = (-1 to 1).map(ColumnTemplate(0,_,WordShapeGenerator,'Shape) )
    val wordClass = (-1 to 1).map(ColumnTemplate(0,_,EnglishWordClassGenerator,'Class) )
    val wordShapeComb = wordShape.combinations(2).map(ProductTemplate(_:_*))
    val wordClassComb = wordClass.combinations(2).map(ProductTemplate(_:_*))

    new CompositeTemplate( (columns ++ backone ++ nextone ++ words ++ wordShape ++ wordClass ++ wordShapeComb ++ wordClassComb):_*)
  } else {
    val template = Source.fromFile(params.template).mkString
    FeatureTemplate.parseTemplate(template)
  }

  val statIndex = Index[SuffStat]()
  val transitionFeatures = Array.tabulate(labelIndex.size,labelIndex.size) { (prev,next) =>
    statIndex.index(LabelStat(prev)) * labelIndex.size + next
  }
  val processed = for(ex <- train) yield ex.map { features =>
    val mapped = IndexedSeq.tabulate(features.length)(i => template.extract(ex, i).map(statIndex.index _).toIndexedSeq)
    mapped -> features.length
  }

  val featureIndex = new Index[CRF.Feature]() {
    def apply(t: CRF.Feature) = t match {
      case Feature(stat,lbl) => lbl + (labelIndex.size * statIndex(stat))
      case _ => -1
    }

    override def size = labelIndex.size * statIndex.size

    def unapply(i: Int) = if(i < 0 || i >= size) None  else {
      val lbl = i % labelIndex.size
      val stat = statIndex.get(i / labelIndex.size)
      Some(Feature(stat,lbl))
    }

    def pairs = (0 until size) map {i => unapply(i).get -> i} iterator

    def iterator = (0 until size) map { get(_)} iterator
  }


  val featurizer = new Featurizer[String,Seq[Seq[Int]]] {
    val index = featureIndex
    def featuresFor(pos: Int, w: Seq[Seq[Int]], l: Int, ln: Int) = {
      w(pos).iterator.map(i => i * labelIndex.size + ln) ++ Iterator(transitionFeatures(l)(ln))
    }
  }

  val trainer = new CRF.Trainer[String, Seq[Seq[Int]]](featurizer, startSymbol, params.opt)
  val crf = trainer.train(processed)

  if(params.output != null) {
    val model = new CRFPackage(crf,template,statIndex)
    breeze.util.writeObject(params.output, model)
  }


  if(params.test != null) {
    val test=
      CONLLSequenceReader.readTrain(new FileInputStream(params.test), params.test.getName).toIndexedSeq

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


}