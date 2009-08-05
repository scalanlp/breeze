import scalanlp.stats._;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import scalanlp.stats.Rand._;
import scalanlp.counters._;
import java.io.File;
import scala.io.Source;
import scalanlp.counters.Counters;
import scala.collection.mutable.Map;
import scala.collection.mutable._;

type RAS[T] = RandomAccessSeq[T];

// topic CRP params
val topicTheta = 0.05;
val topicAlpha = 0.001;

val leftBias = 1;
val rightBias = 1;

val wordSmoothing = 0.1;

case class Sentence(words: RAS[String], topics: RAS[Int]);

case class Params(topicCRP: Map[(Int,Int),PitmanYorProcess], // p(t)
  wCounts: PairedIntCounter[Int,String]) {
  def this() = this(mkBiasedPY(), new PairedIntCounter());
}

def mkBiasedPY() = new HashMap[(Int,Int),PitmanYorProcess](){ 
  override def default(o : (Int,Int)) = o match { case(k1,k2) => 
    val py = new PitmanYorProcess(topicTheta,topicAlpha);
    // HACK:
    if(k2 != -1) {
      py.observe( ((1 to leftBias) map (_ =>  k1)) :_*);
      py.observe( ((1 to rightBias) map (_ =>  k2)) :_*);
      this((k1,k2)) = py;
    } 
    py;
  }
}

println("loading...");
val rawSents = (for( f <- (new File(args(0))).listFiles().iterator;
                    val src = Source.fromFile(f);
                    line <- src.getLines) yield {
                    line.split("[^A-Za-z]").filter(""!=_);
                  }).filter(_.length != 0).toSequence.toArray

val initParams = new Params();
val words = Set[String]();

println("initiailizing...");
val sents = for(s <- rawSents) yield {
  val topics = (new ArrayBuffer[Int]:Buffer[Int]) ++ Array(0,0);  
  for(i <- 0 until s.length) {
    val w = s(i);
    words += w;
    val t2 = topics(i)
    val t1 = topics(i+1)
    val t = initParams.topicCRP( (t2,t1)).get
    initParams.wCounts(t,w) += 1;
    topics += t;
  }
  new Sentence(s,topics.drop(2).toArray);
}

println("sampling");
val numWords = words.size;
words.clear;


def resample(params: Params, s:Sentence) = { 
  val newTopics = resampleTopics(params,s).toArray;
  Sentence(s.words,newTopics);
}

def resampleTopics(params:Params, s:Sentence) = {
  val Sentence(words,topics) = s;
  val newTopics = (new ArrayBuffer[Int]():Buffer[Int]) ++ List(0,0)
  for(i <- 0 until words.length;
      w = words(i);
      t = topics(i);
      pT2 = newTopics(i);
      pT1 = newTopics(i+1)) {
      //unobserve all things having to do with t.
      try {
        params.topicCRP( (pT2,pT1)).unobserve(t);
      } catch {
        case e:Exception => 
          e.printStackTrace;
          println(params.topicCRP);
          println( (pT2,pT1));
          println( params.topicCRP((pT2,pT1)).debugString);
          exit(1);
      }
      params.wCounts(t,w) -= 1;
      if(i+1 < words.length) {
        try {
          params.topicCRP((pT1,t)).unobserve(topics(i+1));
        } catch {
          case e:Exception => 
          e.printStackTrace;
          println(topics);
          println(i);
          println(newTopics);
          println(params.topicCRP);
          println( (pT2,pT1));
          println( params.topicCRP((pT2,pT1)).debugString);
          exit(1);
        }
        if(i+2 < words.length) {
          params.topicCRP((t,topics(i+1))).unobserve(topics(i+2));
        }
      }
      // resample T
      val newT = params.topicCRP((pT2,pT1)).drawWithLikelihood {
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
      if(i+1 < words.length) {
        params.topicCRP((pT1,newT)).observe(topics(i+1));
       // println(params.topicCRP((pT1,newT)).debugString);
       if(i+2 < words.length) {
         params.topicCRP((newT,topics(i+1))).observe(topics(i+2));
       }
      }
      newTopics += newT;
  }
  newTopics.drop(2).toArray;
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

val mc = MarkovChain(initParams) { params => 
  val sentences = sents.map(resample(params,_));
  Rand.fromBody { params }
}

mc.samples.take(100) foreach { case Params(crp,counts) =>
  println(crp.map{ case (k,v) => k + " " + v.debugString}.mkString("{",",","}"));
  println(counts);
}
