package scalanlp.sequences

import scalanlp.optimize.FirstOrderMinimizer
import scalanlp.config.CommandLineParser
import scalanlp.corpora.CONLLSequenceReader
import java.io.{FileInputStream, File}
import scalanlp.util.Index
import scalanlp.sequences.CRF.Featurizer
import collection.immutable.IndexedSeq
import scalanlp.data.{Observation, Example}
import collection.mutable.ArrayBuffer
import scalanlp.stats.ContingencyStats

/**
 * 
 * @author dlwh
 */
object CRFMain extends App {
  case class Params(opt: FirstOrderMinimizer.OptParams,
                    train: File,
                    test: File,
                    testHasGold: Boolean = true)

  val config = CommandLineParser.parseArguments(args)._1
  val params = config.readIn[Params]("")
  val train = CONLLSequenceReader.readTrain(new FileInputStream(params.train), params.train.getName).toIndexedSeq
  val labelIndex = Index(Iterator("START") ++ {for(ex <- train.iterator; l <- ex.label) yield l})

  trait SuffStat
  case class ColumnStat(column: Int, offset: Int, value: Any) extends SuffStat
  case class LabelStat(lbl: Int) extends SuffStat
  case class ProductStat(stats: Seq[SuffStat]) extends SuffStat
  case class Ngram(seq: String) extends SuffStat

  case class Feature(stat: SuffStat, lbl: Int) extends CRF.Feature

  import scala.collection.IndexedSeq

  trait Template {
    def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int):Iterator[SuffStat]
  }

  case class ColumnTemplate(column: Int, offset: Int, transform: String=>Any = identity) extends Template {
    def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int) = {
      val realpos = pos + offset
      import ex._
      if(realpos >= 0 && realpos < features.length) {
//        println(features(realpos).length,column)
        if(features(realpos).length <= column) error("WTF:" + features(realpos) + " " + column + " " + ex + " " )
        Iterator(ColumnStat(column,offset,transform(features(realpos)(column))))
      } else Iterator.empty
    }
  }

  case class ProductTemplate(templates: Template*) extends Template {
    def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int) = {
      val all = templates.map(_.extract(ex,pos).toIndexedSeq).toList
      def rec(current: List[IndexedSeq[SuffStat]]):IndexedSeq[List[SuffStat]] = {
        if(current.isEmpty) IndexedSeq(List.empty)
        else {
          val rest = rec(current.tail)
          for(k <- current.head; l <- rest) yield k +: l
        }
      }
      rec(all).iterator.map(ProductStat(_))
    }
  }

  case class NgramTemplate(column: Int = 0, order: Int = 4) extends Template {
    def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int) = {
      val word = ex.features.apply(pos)(column)
      val bracketed  = "#" + word +"#"
      val pref = for(i <- 2 until order if bracketed.length <= order) yield Ngram(bracketed.substring(0,i))
      val suff = for(i <- 2 until order if bracketed.length > i) yield Ngram(bracketed.substring(bracketed.length-i))
      pref ++ suff iterator
    }
  }

  class Composite(templates: Template*) extends Template{
    def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int) = {
      templates.iterator.flatMap(_.extract(ex,pos))
    }
  }

  val columns = (0 until train(0).features.apply(0).length) map(ColumnTemplate(_,0))
  val backone = (0 until train(0).features.apply(0).length) map(ColumnTemplate(_,-1))
  val nextone = (0 until train(0).features.apply(0).length) map(ColumnTemplate(_,1))
  val words = Seq(ColumnTemplate(0,-2),ColumnTemplate(0,2),NgramTemplate(),
    ProductTemplate(ColumnTemplate(0,-1),ColumnTemplate(0,1)),
    ProductTemplate(ColumnTemplate(0,0),ColumnTemplate(0,1)),
    ProductTemplate(ColumnTemplate(1,-1),ColumnTemplate(1,1)),
    ProductTemplate(ColumnTemplate(1,0),ColumnTemplate(1,1))
  )

  val template = new Composite( (columns ++ backone ++ nextone ++ words):_*)

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

  val trainer = new CRF.Trainer[String, Seq[Seq[Int]]](featurizer, "START", params.opt)
  val crf = trainer.train(processed)
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