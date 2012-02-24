package scalanlp.sequences

import scalanlp.data.Observation
import scala.collection.IndexedSeq
import util.parsing.combinator.JavaTokenParsers
import scalanlp.text.tokenize.{WordShapeGenerator, EnglishWordClassGenerator}

/**
 * A SuffStat is a feature in a CRF without any information about the current label
 *
 * @author dlwh
 */
trait SuffStat extends Serializable

case class ColumnStat(column: Int, offset: Int, value: Any, tag: Symbol = 'Value) extends SuffStat
case class LabelStat(lbl: Int) extends SuffStat
case class ProductStat(stats: Seq[SuffStat]) extends SuffStat

/**
 * A feature template is capable of extracting a set of SuffStats from a sequence model
 * @author dlwh
 */
trait FeatureTemplate extends Serializable {
  def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int):Iterator[SuffStat]
}

object FeatureTemplate {
  def parseTemplate(repr: String):FeatureTemplate = {
    import Parser._
    new CompositeTemplate(parseAll(rep(template), repr).get:_*)
  }

  object Parser extends JavaTokenParsers {
    lazy val template:Parser[FeatureTemplate] = (column | ngram | product)

    lazy val column = (
      ("(" | "column(") ~> ((wholeNumber <~ ',') ~ wholeNumber ~ opt(',' ~> transform ~ opt(',' ~> tag))) <~ ")" ^^ { case (column ~ offset ~ stuff) =>
        val (transform,tag) = stuff match {
          case Some(x ~ Some(tag)) => x -> tag
          case Some(x ~ None) => x -> 'Value
          case None => identity[String]_ -> 'Value
        }
        ColumnTemplate(column.toInt, offset.toInt,transform,tag)
      }
      )

    lazy val ngram = ("n(" | "ngram(") ~> ((wholeNumber <~ ',') ~ wholeNumber ~ opt(',' ~> wholeNumber)) ^^ { case (column ~ offset ~ order) =>
      NgramTemplate(column.toInt,offset.toInt,order = order.getOrElse("8").toInt)
    }

    lazy val product = ("product(" | "p(") ~> repsep(template,",") <~ ")" ^^ { case templates => ProductTemplate(templates:_*)}

    lazy val transform = (
      "identity" ^^^ { identity[String] _}
    | "class" ^^^ { EnglishWordClassGenerator}
    | "shape" ^^^ { WordShapeGenerator}
    | "named" ~> rep1sep(ident,".") ^^ { name => Class.forName(name.reduceLeft(_ + "." + _)).asInstanceOf[String=>Any] }
    )


    lazy val tag = ident ^^ {Symbol(_)}
  }
}

/**
 * A Column template
 */
case class ColumnTemplate(column: Int, offset: Int,
                          transform: String=>Any = identity,
                          tag:Symbol= 'Value) extends FeatureTemplate {
  def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int) = {
    val realpos = pos + offset
    import ex._
    if(realpos >= 0 && realpos < features.length) {
      Iterator(ColumnStat(column,offset,transform(features(realpos)(column)), tag))
    } else Iterator.empty
  }
}

case class ProductTemplate(templates: FeatureTemplate*) extends FeatureTemplate {
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

case class NgramTemplate(column: Int = 0, offset: Int = 0, order: Int = 8) extends FeatureTemplate {
  def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int) = {
    val realpos = pos + offset
    if(realpos >= 0 && realpos < ex.features.length) {
      val word = ex.features.apply(realpos)(column)
      val bracketed  = "#" + word +"#"
      val pref = for(i <- 2 until order if i <= bracketed.length ) yield {
        ColumnStat(column, offset, bracketed.substring(0,i), 'Prefix)
      }
      val suff = for(i <- 2 until order if bracketed.length > i) yield {
        ColumnStat(column, offset, bracketed.substring(bracketed.length-i), 'Suffix)
      }

      pref ++ suff iterator
    } else {
      Iterator.empty
    }
  }
}

class CompositeTemplate(templates: FeatureTemplate*) extends FeatureTemplate{
  def extract(ex: Observation[IndexedSeq[IndexedSeq[String]]], pos: Int) = {
    templates.iterator.flatMap(_.extract(ex,pos))
  }
}