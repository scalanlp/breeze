package scalanlp.stats;

import scalanlp.counters.ints.Int2DoubleCounter;
import scalanlp.counters._;
import scalanlp.counters.Counters._;
import util._;
import scala.collection.mutable._;
import scalanlp.collection.mutable.ArrayMap;

/**
* Models the CRP over the non-negative integers. 
*
* @param theta the prior probability of a new class
* @param alpha the amount of discounting to current draws.
*
* @author dlwh
*/
class PitmanYorProcess(val theta: Double, val alpha:Double) extends Distribution[Int] { outer =>
  def this(theta: Double) = this(theta,0.0);

  assert( (alpha < 0 && theta % alpha == 0.0) || (0 <= alpha && alpha <= 1.0 && theta > -alpha));
  
  val drawn = new ArrayMap[Double] { override def defaultValue = 0.0 };
  drawn(0) = theta;
  private var total = theta;

  

  override def get() = getWithArrayBuffer(drawn,total);

  private var c = -1; 
  def numClasses = nclasses;
  private var nclasses = 0;

  private def nextClass = {
    do { c += 1; } while(drawn.get(c) != None)
    nclasses += 1;
    c;
  }

  private def getWithArrayBuffer(cn : ArrayMap[Double],tot:Double) = {
    val d : Int = Multinomial(cn.innerArray,tot).get match {
      case 0 => nextClass;
      case x => x;
    }

    if(drawn(d) == 0) {
      drawn(d) = 1- alpha;
      drawn(0) += alpha;
    } else {
      drawn(d) += 1;
    }
    total += 1;

    d - 1  
  }
  
  def probabilityOf(e: Int) = {
    if (e >= 0 && drawn.get(e+1) != None) {
      drawn(e+1) / total;
    } else {
      0.0;
    }
  }

  def probabilityOfUnobserved() = drawn(0) / total;

  def observe(c: IntCounter[Int]) {
    for( (k,v) <- c) {
      if(k < 0) throw new IllegalArgumentException(k + " is not a valid draw from the PitmanYorProcess");
      if(v != 0)
        observeOne(k,v);
    }
  }

  private def observeOne(k_ : Int, v: Int) {
    val k = k_ + 1;
    var newCount = drawn(k) + v;
    if(drawn(k) == 0 && v != 0) {
      newCount -= alpha;
      drawn(k) += alpha;
    }

    if( newCount < -alpha) { 
      throw new IllegalArgumentException("Class " +k + "would be reduced below 0 count!" + newCount);
    } else if( Math.abs(newCount) < alpha) {
      total -= drawn(k) + alpha;
      drawn(k) = 0;
      drawn(0) -= alpha;
    } else {
      total += drawn(k) + v;
      drawn(k) = newCount;
    }
  }

  def observe(t : Int*) { observe(count(t))}
  def unobserve(t: Int*) {
    val c = count(t);
    c.transform { (k,v) => v * -1};
    observe(c);
  }

  def withLikelihood(p : Option[Int]=>Double) = new Rand[Int] {
    def get = {
      var total = 0.0;
      val c2 = new ArrayMap[Double]();
      for( (k,v) <- drawn) {
        c2(k) = (v * p(if(k == 0) None else Some(k-1)));
        total += c2(k);
      }
      getWithArrayBuffer(c2,total);
    }
  }

  def drawWithLikelihood(p: Option[Int]=>Double) = withLikelihood(p).get;

  class Mapped[T](r: Rand[T]) extends Distribution[T] {
  val forward = new HashMap[Int,T] {
      override def default(k:Int) ={
        val draw = r.get;
        update(k,draw);
        backward(draw) += k;
        draw;
      }
    }

    val backward = new HashMap[T,ArrayBuffer[Int]]() {
      override def default(k:T) = {
        this.getOrElseUpdate(k,new ArrayBuffer[Int]);
      }
    }

    def get = { 
      forward(outer.get);
    }

    def observe(t : T*) { observe(count(t))}
    def unobserve(t: T*) {
      val c = count(t);
      c.transform { (k,v) => v * -1};
      observe(c);
    }

    def observe(c: IntCounter[T]) {
      for( (k,v) <- c) {
        if(v != 0) {
          backward.get(k) match {
            case None => 
              if(v < 0)  {
                throw new IllegalArgumentException(k + " is not a valid draw from the PitmanYorProcess");
              } else {
                val n = nextClass;
                forward(n) = k;
                backward(k) += v;
                observeOne(n,v);
              }

            case Some(buf) if buf.length == 0 => 
              if(v < 0)  {
                throw new IllegalArgumentException(k + " is not a valid draw from the PitmanYorProcess");
              } else {
                val n = nextClass;
                forward(n) = k;
                backward(k) += v;
                observeOne(n,v);
              }

            case Some(buf) if buf.length == 1 =>
              observeOne(buf(0),v); 

            case Some(buf) =>
              val sign = v < 0;
              for(i <- 1 to v.abs) {
                val idx = Multinomial(buf.map(outer probabilityOf _).toArray).get; 
                observeOne(buf(idx), if(sign) -1 else 1);
                if(outer.probabilityOf(buf(idx)) == 0) buf -= idx;
              }
          }
        }
      }
    }


    def probabilityOf(t: T) = backward(t).map(outer.probabilityOf _).foldLeft(0.0)(_+_);
  }

  def withBaseMeasure[T](r: Rand[T])= new Mapped[T](r);
  
  def withBaseMeasure[T](r: Distribution[T]) = new Mapped[T](r) {
    override def probabilityOf(t: T) = {
      val fromDraws = backward(t).map(outer.probabilityOf _).foldLeft(0.0)(_+_);
      val pDraw = r.probabilityOf(t) 
        pDraw * outer.probabilityOfUnobserved + fromDraws;
    }
  }



  override def toString() = {
    "PY(" + theta + "," + alpha + ")";
  }

  def debugString() = {
    val str = drawn.elements.filter(_._1 != 0).map(kv => (kv._1 -1)+ " -> " + kv._2).mkString("draws = (", ", ", ")");
    toString + "\n{newClass=" + drawn(0) + ", " + str + "}";
  }

}
