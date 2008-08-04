import scalanlp.stats._;
import scalanlp.stats.Rand._;
import scalanlp.counters._;
import java.io.File;
import scala.io.File;
import scalanlp.counters.Counters;

case class Params(wordCounts : Array[IntCounter[String]]);

val K = 20;
val wordSmoothing = 0.05;
val topicSmoothing = 50.0/K;
case class Doc(words : Array[String]) {
  var topics = new Array[Int](words.length);
  val topicCounts = new Array[Int](K);
}

val docs = (new File(args(0))).listFiles().map(Source.fromFile.getLines()).map{ lines =>
 lines.toList.toArray.flatMap(_.split("[^A-Za-z]"));
}

def init(docs : List[Doc])  = {
  val wordCounts = Array.fromFunction[IntCounter[String]]( (x:Int) => IntCounter[String]())(K);
  val allWords = scala.collection.mutable.Set[String]();
  val uniform = randInt(K);
  for(doc <- docs) {
    for(i <- 0 until doc.words.length) {
      doc.topics(i) = randInt.get(); 
      wordCounts(doc.topics(i))(doc.words(i)) += 1;
      allWords += doc.words(i);
      doc.topicCounts(doc.topics(i))+=1;
    }
  }
  (allWords.size, wordCounts);
}

val (v,wordCounts) = init(docs);

def resampleTopic(d : Doc,wordCounts : Array[IntCounter[String]] ) = { tuple:(Int,String) =>
  val (z,w) = tuple;
  Rand.fromBody {
    d.topicCounts(z) -=1;
    wordCounts(z)(w) -=1;
    val arr = new Array[Double](K);
    var total = 0.0;
    for(j <- 0 until K) {
      val r =   (d.topicCounts(j) + topicSmoothing) *
      (wordCounts(j)(w) + wordSmoothing) /
      (wordCounts(j).total + wordSmoothing * v);
      arr(j) =  r;
      total += r;
    }
    val zNew = Multinomial(arr,total).get();

    d.topicCounts(zNew) +=1;
    wordCounts(zNew)(w) +=1;
    zNew
  }
}

def resample(wordCounts: Array[IntCounter[String]])(d : Doc): Rand[Doc] = {
  for(newTopics <- promote(d.topics.zip(d.words).map(resampleTopic(d,wordCounts))))
    yield { 
      d.topics = newTopics;
      d
    }
}

val mc = MarkovChain(Params(wordCounts)) { case Params(wordCounts) =>
  val randDocs : List[Rand[Doc]] = docs.map(resample(wordCounts)_);
  for(newDocs <- promote(randDocs))
    yield Params(wordCounts);
}
val chain = mc.sample(100);
