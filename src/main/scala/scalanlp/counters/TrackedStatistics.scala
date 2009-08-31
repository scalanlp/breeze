package scalanlp.counters;

import scala.collection.mutable.ArrayBuffer;

import scalala.tensor._;

import scalanlp._;

/**
* Mixin for tracking various statistics in things like counters.
*
* @author dlwh
*/
trait TrackedStatistics[T] { 
  /**
  * The statistics we're tracking. Subtraits should add functions of the form
  * (T,newValue,oldValue)=&gt;Unit in their constructor.
  */
  protected val statistics  = new ArrayBuffer[(T,Double,Double)=>Unit]

  /**
  * Called by implementing classes when a new key's value is changed in some way.
  */
  protected[counters] final def updateStatistics(t: T, oldV: Double, newV: Double) {
    statistics foreach ( _.apply(t,oldV,newV) );
  }
}

object TrackedStatistics {
  /**
  * Tracks the sum of values in the tracked object.
  */
  trait Total[T] extends TrackedStatistics[T] {
    def total = total_;

    private var total_ = 0.0;

    statistics += { (t :T, oldV: Double, newV: Double) =>
      total_ += (newV - oldV);
    }
  }

  /**
  * Tracks the log of sum of values in the tracked object.
  */
  trait LogTotal[T] extends TrackedStatistics[T] {
    def logTotal = logTotal_;
    private var logTotal_ = 0.0;
    statistics += { (t :T, oldV: Double, newV: Double) =>
      logTotal_ = math.Numerics.logSum(logTotal_, (newV - oldV));
    }
  }
}

/**
* Mixin for tracking various statistics in things like counters.
*
* @author dlwh
*/
trait TrackedIntStatistics[T] { 
  /**
  * The statistics we're tracking. Subtraits should add functions of the form
  * (T,newValue,oldValue)=&gt;Unit in their constructor.
  */
  protected val statistics  = new ArrayBuffer[(T,Int,Int)=>Unit]

  /**
  * Called by implementing classes when a new key's value is changed in some way.
  */
  protected[counters] final def updateStatistics(t: T, oldV: Int, newV: Int) {
    statistics foreach ( _.apply(t,oldV,newV) );
  }
}

object TrackedIntStatistics {
  /**
  * Tracks the sum of values in the tracked object.
  */
  trait Total[T] extends TrackedIntStatistics[T] {
    def total = total_;

    private var total_ = 0.0;

    statistics += { (t :T, oldV: Int, newV: Int) =>
      total_ += (newV - oldV);
    }
  }

}
