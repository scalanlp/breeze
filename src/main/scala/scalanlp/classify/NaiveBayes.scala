package scalanlp.classify;
import counters._;
import Math._;
import data._;

class NaiveBayes[W,L](c: =>Collection[Example[L,W,Int]],
    val wordSmoothing:Double,
    val classSmoothing:Double)  extends Classifier[L,W,Int] {

  private val classCounts = IntCounter[L]();
  private val wordCounts = new PairedIntCounter[L,W]();

  private val vocabSize = train();

  private def train() = {
    val myC = c;
    val allWords = scala.collection.mutable.Set[W]();
    for(e <- myC) {
       classCounts(e.label) += 1;
       wordCounts(e.label) ++= e.features;
       allWords ++= e.features.keys;
    }
    allWords.size;
  }

  def scores(o : Observation[W,Int]) = {
    val res = DoubleCounter[L]();
    for( (l,prior) <- classCounts) {
      res(l) += log(prior + classSmoothing);
      val probWC = wordCounts(l);
      val logDenom = log(probWC.total  + vocabSize * wordSmoothing);
      val logWordProbabilities = o.features.map{ case (k,v) => v * (log(probWC(k) + wordSmoothing) - logDenom)}
      res(l) += logWordProbabilities.foldLeft(0.0)(_+_);
    }
    res;
  }
}



class BinomialNaiveBayes[W,L](c: =>Collection[Example[L,W,Int]],
    val wordSmoothing:Double,
    val classSmoothing:Double)  extends Classifier[L,W,Int] {

  private val classCounts = IntCounter[L]();
  private val wordCounts = new PairedIntCounter[L,W]();

  private val vocabSize = train();

  private def train() = {
    val myC = c;
    val allWords = scala.collection.mutable.Set[W]();
    for(e <- myC) {
       classCounts(e.label) += 1;
       wordCounts(e.label) ++= e.features;
       allWords ++= e.features.keys;
    }
    allWords.size;
  }

  def scores(o : Observation[W,Int]) = {
    val res = DoubleCounter[L]();
    for( (l,prior) <- classCounts) {
      res(l) += log(prior + classSmoothing);
      val probWC = wordCounts(l);
      val logDenom = log(probWC.total  + vocabSize * wordSmoothing);
      val logWordProbabilities = o.features.map{ case (k,v) => v * (log(probWC(k) + wordSmoothing) - logDenom)}
      res(l) += logWordProbabilities.foldLeft(0.0)(_+_);
    }
    res;
  }
}

object NaiveBayes {
  def main(args : Array[String]) {
    import java.io._;
    import scalanlp.data._;
    val trainData = 
      for( dir <- new File(args(0)).listFiles;
      file <- dir.listFiles)
    yield Bag.fromFile(file);

    val testData = for( dir <- new File(args(1)).listFiles;
      file <- dir.listFiles)
    yield Bag.fromFile(file);

    val nb = new NaiveBayes(trainData,3,0.1);

    val trainRight = for(ex <- trainData;
      label = nb(ex))
    yield if(label == ex.label) 1.0 else 0.0;

    val trainAcc = trainRight.reduceLeft(_+_) / trainData.size;

    val testRight = for(ex <- testData;
      label = nb(ex))
    yield if(label == ex.label) 1.0 else 0.0;

    val testAcc = testRight.reduceLeft(_+_) / testData.size;

    println(trainAcc + " " + testAcc);
  }
}
