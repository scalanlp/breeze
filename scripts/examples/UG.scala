import scalanlp.stats._;
import scalanlp.stats.Rand._;
import scalanlp.counters._;
import java.io.File;
import scala.io.File;
import scalanlp.counters.Counters;
import scalanlp.collection.mutable.Map;
import scalanlp.collection.mutable.ArrayBuffer;

type RAS[T] = RandomAccessSeq[T];

// topic CRP params
val topicTheta = 0.5;
val topicAlpha = 0.1;

val leftBias = 3;
val rightBias = 3;

val wordSmoothing = 0.1;

case class Sentence(words: RAS[String], topics: RAS[Int]);

case class Params(topicCRP: Map[(Int,Int),PitmanYorProcess], // p(t)
  wCounts: PairedIntCounter[Int,String]) {
  def this() = this(mkBiasedPY(), new PairedIntCounter());
}

def mkBiasedPY() = new HashMap[(Int,Int),PitmanYorProcess]().withDefault{ case(k1,k2) => 
  val py = new PitmanYorProcess(topicTheta,topicAlpha);
  py.observe( ((1 to leftBias) map (_ =>  k1)) :_*);
  py.observe( ((1 to rightBias) map (_ =>  k2)) :_*);
  // HACK:
  if(k2 != -1) this((k1,k2) = py;
  py;
}


def resample(params: Params, s:Sentence) = { 
  val newTopics = resampleTopics(params,s).toArray;
  Sentence(words,newTopics);
}

def resampleTopics(params:Params, s:Sentence) = {
  val Sentence(words,topics,edges) = s;
  val previousTs = new ArrayBuffer[Int]() + 0 + 0;
  for(i <- 0 until words.length;
      w = words(i);
      t = topics(i)
      pT1 = previousTs(i+1)
      pT2 = previousTs(i)) {
      //unobserve all things having to do with t.
      params.topicCRP( (pT1,pT2)).unobserve(t);
      params.wCounts(t,w) -= 1;
      // resample T
      val newT = params.topicCRP((pT1,pT2)).drawWithLikelihood {
        case Some(tNew) => 
          val pWordGivenT = (params.wCounts(tNew)(w) + wordSmoothing) / (params.wCounts(tNew).total + wordSmoothing * numWords);
          if(i+1 < words.length) {
            val pTnextGivenT= probNextTopic(params,pT1,tNew,topics(i+1));
            pWordGivenT * pTnextGivenT;
          } else {
            pWordGivenT;
          }
        case None => 
          val pWordGivenT = 1. / numWords;
          if(i+1 < words.length) {
            val pTnextGivenT= probNextTopic(params,pT1,-1,topics(i+1));
            pWordGivenT * pTnextGivenT;
          } else {
            pWordGivenT;
          }
      };
      params.wCounts(newT,w) += 1;
      previousTs += newT;
  }
  previousTs.drop(2).toArray;
}

def probNextTopic(params: Params, pT1: Int, pT:Int, t:Int) = {
  val pN = params.topicCRP( (pT1,pT)).probabilityOf(t);
  if(pN == 0.0) {
    params.topicCRP( (pT1,pT)).probabilityOfUnobserved();
  } else {
    pN
  }
}

// Main

val rawSents = (new File(args(0))).listFiles().map(Source.fromFile.getLines()).flatMap{ lines =>
 (Array[String]() ++ lines).map(_.split("[^A-Za-z]")).filter(""==_);
}

val initParams = new Params();

val sents = for(s <- rawSents) yield {
  val topics = new ArrayBuffer[Int] ++ Array(0,0);  
  for(i <- 0 until s.length) {
    val w = s(i);
    val t1 = topics(i)
    val t2 = topics(i+1)
    val t = initParams.topicCRP( (t1,t2)).get
    topics.wCounts(t,w) += 1;
    topics += t;
  }
  new Sentence(s,topics.drop(2).toArray);
}


val mc = MarkovChain(initParams) { params => 
  val sentences = sentences.map(resample(params,_));
  Rand.fromBody { params }
}
