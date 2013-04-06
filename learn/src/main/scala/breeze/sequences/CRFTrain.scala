package breeze.sequences

import breeze.optimize.FirstOrderMinimizer
import breeze.config.CommandLineParser
import breeze.corpora.CONLLSequenceReader
import java.io.{FileInputStream, File}
import breeze.util.Index
import breeze.sequences.CRF.Featurizer
import collection.mutable.ArrayBuffer
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import io.Source
import breeze.data.{Example, Observation}
import breeze.linalg._

/**
 * This class is just for holding onto all the information produced by CRFTrain.
 *
 * CRFTest can process it.
 *
 * @param crf the crf
 * @param template the feature template created using this CRF
 * @param statIndex the index for features (features without labels)
 */
case class CRFPackage(crf: CRF[String,Seq[SparseVector[Double]]],
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

  def process(ex: Observation[IndexedSeq[IndexedSeq[String]]]): (IndexedSeq[SparseVector[Double]], Int) = {
    val mapped = IndexedSeq.tabulate(ex.features.length){i =>
      val stats = SparseVector.zeros[Double](statIndex.size)
      for(stat <- template.extract(ex, i)) {
        val istat = statIndex(stat)
        if(istat >= 0) {
          stats(istat) = 1
        }
      }
      stats
    }
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

  // index all stats for all sequences, then
  // make vectors from the sequences
  val vectors = {

    val processed = for(ex <- train) yield ex.map { features =>
      val mapped = IndexedSeq.tabulate(features.length)(i => template.extract(ex, i).map(statIndex.index _).toIndexedSeq)
      mapped -> features.length
    }

    for(ex <- processed) yield ex.map[(IndexedSeq[SparseVector[Double]], Int)] { case (indexedStats, length) =>
      val asVectors = indexedStats.map { stats =>
        val vec = SparseVector.zeros[Double](statIndex.size)
        for( i <- stats) {
          vec(i) += 1.0
        }
        vec
      }
      asVectors -> length
    }
  }

  val featureIndex = new Index[CRF.Feature]() {
    def apply(t: CRF.Feature) = t match {
      case Feature(stat,lbl) => lbl + (labelIndex.size * statIndex(stat))
      case _ => -1
    }

    override val size = labelIndex.size * statIndex.size

    def unapply(i: Int) = if(i < 0 || i >= size) None  else {
      val lbl = i % labelIndex.size
      val stat = statIndex.get(i / labelIndex.size)
      Some(Feature(stat,lbl))
    }

    def pairs = (0 until size) map {i => unapply(i).get -> i} iterator

    def iterator = (0 until size) map { get(_)} iterator
  }


  val featurizer = new Featurizer[String,Seq[SparseVector[Double]]] {
    val index = featureIndex
    // expands the vector of sufficient statistics to one over features, this is
    // accomplished by converting the indices in the vector to the right value for the
    // feature vector
    def featuresFor(pos: Int, w: Seq[SparseVector[Double]], l: Int, ln: Int) = {
      val stats = w(pos)
      // TODO: have code for initial non-zeros in SparseVector's companion
      val expandedIndex = new Array[Int](stats.activeSize+1)
      val expandedData = new Array[Double](stats.activeSize+1)
      expandedIndex(0) = (transitionFeatures(l)(ln))
      expandedData(0) = 1.0
      var off = 0
      while(off < stats.activeSize) {
        val i = stats.index(off)
        val v = stats.data(off)
        expandedIndex(off+1) = (i * labelIndex.size + ln)
        expandedData(off+1) = v
        off += 1
      }
      val expanded = new SparseVector[Double]( expandedIndex, expandedData, expandedIndex.size, featureIndex.size)
      expanded
    }
  }

  val trainer = new CRF.Trainer[String, Seq[SparseVector[Double]]](featurizer, startSymbol, params.opt)
  val crf = trainer.train(vectors)

  val model = new CRFPackage(crf,template,statIndex)

  if(params.output != null) {
    breeze.util.writeObject(params.output, model)
  }


  if(params.test != null) {
    val test =
      CONLLSequenceReader.readTrain(new FileInputStream(params.test), params.test.getName).toIndexedSeq

    val testProcessed = for(ex <- test) yield ex.map{_ => model.process(ex)}

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
