package breeze.corpora

import com.codecommit.antixml._

/**
 * Convert native MASC xml into CONLL format for named entity recognition.
 *
 * @author jasonbaldridge
 */
object MascNer {

  import io.Source
  import java.io._
  import MascUtil._

  lazy val outsideNe = MAnnotation("outside", "outside", "none", Map[String,String]())

  lazy val nerLabelStandardizer = Map(
    "location" -> "LOC",
    "person" -> "PER",
    "org" -> "ORG",
    "date" -> "DAT"
  ).withDefault(x=>"O")

  def main(args: Array[String]) {
    val mascDir = args(0)
    val targets = collectTargets(new File(mascDir))

    // Get 3/5 for train, 1/5 for dev, and 1/5 for test
    val targetsAndIndices = targets.zipWithIndex
    processSet("train", targetsAndIndices.filter(_._2 % 5 < 3).unzip._1)
    processSet("dev", targetsAndIndices.filter(_._2 % 5 == 3).unzip._1)
    processSet("test", targetsAndIndices.filter(_._2 % 5 == 4).unzip._1)
  }

  def collectTargets(dir: File): Seq[(File,String)] = {
    val files = dir.listFiles.toSeq
    val filesInDir = files.filter(_.getName.endsWith(".txt"))
      .map(file => (dir, file.getName.dropRight(4)))
    filesInDir ++ files.filter(_.isDirectory).flatMap(collectTargets)
  }

  def processSet(outputName: String, targets: Seq[(File, String)]) {
    System.err.println("Creating " + outputName)
    val output = new FileWriter(outputName)
    for ((file, prefix) <- targets) {
      try {
        val allDataBySentence = processTarget(file,prefix)
        for (sentenceInfo <- allDataBySentence) {
          sentenceInfo.zipped.foreach { 
            case (tok, pos, ner) => output.write(tok + " " + pos + " " + ner + "\n")
          }
          output.write("\n")
        }
        System.err.println("Success: " + file + "," + prefix)
      } 
      catch { 
        case e => System.err.println("Failure: " + file + "," + prefix)
      }
    }
    output.flush
    output.close
    System.err.println
  }

  def processTarget(dir: File, prefix: String) = {

    def dirFile(prefix: String) = new File(dir, prefix)

    // Raw text
    val rawtext = Source.fromFile(dirFile(prefix+".txt")).mkString

    // Sentence information
    val sentenceXml = XML.fromSource(Source.fromFile(dirFile(prefix+"-s.xml")))
    val sentenceRegions = getRegions(sentenceXml).sorted

    // Basic segment information
    val segmentXml = XML.fromSource(Source.fromFile(dirFile(prefix+"-seg.xml")))
    val segmentRegions = getRegions(segmentXml).map(r => (r.id -> r)).toMap

    // POS information
    val pennXml = XML.fromSource(Source.fromFile(dirFile(prefix+"-penn.xml")))

    val tokenRegions =  getNodes(pennXml).map { n =>
      val regions = n.targets.map(segmentRegions).sorted
      (n.id -> MRegion(n.id, regions.head.start, regions.last.end))
    }.toMap

    val tokens = tokenRegions.mapValues(region => rawtext.slice(region.start, region.end))
    val posAnnotations = getAnnotations(pennXml).map(anno => (anno.ref -> anno)).toMap

    // NER information
    val neXml = XML.fromSource(Source.fromFile(dirFile(prefix+"-ne.xml")))
    val neAnnotations = 
      getAnnotations(neXml).map(anno => (anno.ref -> anno)).toMap.withDefault(x=>outsideNe)

    val neEdges = 
      getEdges(neXml).map(edge => (edge.to -> edge.from)).toMap.withDefault(x=>"outside")

    // A helper function for pulling out the information associated with a 
    // subsequence of the tokens in the document. 
    def orderedTokPosNer(orderedRegions: Seq[MRegion]) = {
      if (orderedRegions.length == 0) None
      else {
        val orderedTokens = orderedRegions.map(reg=>tokens(reg.id))
        
        val (orderedPos, orderedNe) = orderedRegions.map { region => {
          val posAnno = posAnnotations(region.id)
          val neAnno = neAnnotations(neEdges(posAnno.ref))
          (getPos(posAnno), neAnno)
        }}.unzip
        
        val bioLabels = (outsideNe +: orderedNe).sliding(2).toSeq.map { 
          case Seq(prev, curr) =>
            if (curr.label == "outside")
              nerLabelStandardizer(curr.label)
            else {
              val prefix = if (prev.id != curr.id) "B-" else "I-"
              prefix+nerLabelStandardizer(curr.label)
            }
        }
        Some(orderedTokens, orderedPos, bioLabels)
      }
    }


    // Insert the "missing" sentences. (Content not marked as a sentence, 
    // but containing tokens.)
    val paddedSentenceRegionBuffer = 
      collection.mutable.ListBuffer[MRegion](sentenceRegions.head)

    sentenceRegions.sliding(2).foreach { case Seq(prev, curr) => {
      if (prev.end+1 < curr.start)
        paddedSentenceRegionBuffer.append(MRegion("", prev.end+1, curr.start-1))
      paddedSentenceRegionBuffer.append(curr)
    }}
    
    val paddedSentenceRegions = paddedSentenceRegionBuffer.toSeq

    // Pull out the sequence of token, pos, and NE for each sentence.
    val allOrderedTokRegions = tokenRegions.values.toIndexedSeq.sorted
    var index = 0
    val allDataBySentence = paddedSentenceRegions.flatMap { region => {
      val endIndex = allOrderedTokRegions.indexWhere(t=>t.end>region.end,index)
      if (index == endIndex) None
      else {
        val sentence = allOrderedTokRegions.slice(index,endIndex)
        index = endIndex
        orderedTokPosNer(sentence)
      }
    }}
    
    allDataBySentence

  }

}

/**
 * Simple objects and functions for working with MASC data.
 *
 * @author jasonbaldridge
 */
object MascUtil {

  case class MNode(id: String, targets: Seq[String])
  case class MAnnotation(id: String, label: String, ref: String, features: Map[String,String])
  case class MEdge(id: String, from: String, to: String)
  case class MRegion(id: String, start: Int, end: Int) extends Ordered[MRegion] {
    def compare(that: MRegion) = this.start - that.start
  }

  val idQname = QName(Some("xml"),"id")

  def getRegions(doc: Elem) = (doc \\ "region").toSeq.map { rxml =>
    val Array(start, end) = rxml.attrs("anchors").split(" ")
    MRegion(rxml.attrs(idQname), start.toInt, end.toInt)
  }
    
  def getNodes(doc: Elem) = (doc \\ "node").toSeq.map { nxml =>
    val targets = (nxml \ "link").head.attrs("targets").split(" ").toSeq
    MNode(nxml.attrs(idQname), targets)
  }
  
  def getEdges(doc: Elem) = (doc \\ "edge").toSeq
    .map(exml => MEdge(exml.attrs(idQname), exml.attrs("from"), exml.attrs("to")))
  
  def getAnnotations(doc: Elem) = (doc \\ "a").toSeq.map { axml => 
    val features = (axml \\ "f").toSeq
      .map(fnode => (fnode.attrs("name") -> fnode.children.toString)).toMap
    MAnnotation(axml.attrs(idQname),axml.attrs("label"),axml.attrs("ref"), features)
  }

  // Have to go through some pains to make sure we get a POS for every token.
  def getPos(anno: MAnnotation) = {
    if (anno.features.isDefinedAt("msd")) anno.features("msd")
    else if (anno.features.get("kind").getOrElse("") == "urlAddress") "URL"
    else if (anno.features.isDefinedAt("categor")) anno.features("categor")
    else "UNK"
  }

}
