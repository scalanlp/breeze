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
  * Called by implementing classes when a new key's value is changed in some way.
  */
  protected[counters] def updateStatistics(t: T, oldV: Double, newV: Double) {
  }

  protected[counters] def resetStatistics() {
  }

}

object TrackedStatistics {
  /**
  * Tracks the sum of values in the tracked object.
  */
  trait Total[T] extends TrackedStatistics[T] {
    def total = total_;

    private var total_ = 0.0;

    override protected[counters] def updateStatistics(t :T, oldV: Double, newV: Double) = {
      total_ += (newV - oldV);
      super.updateStatistics(t,oldV,newV);
    }

    override protected[counters] def resetStatistics() {
      total_ = 0
      super.resetStatistics();
    }
  }

  /**
   * Tracks the minimum of the values in the counter.
   * @author dramage
   */
  trait Minimum[T] extends TrackedStatistics[T] {
    private var _minK : Option[T] = None;
    private var _minV : Double = Double.MaxValue;

    override protected[counters] def updateStatistics(t : T, oldV : Double, newV : Double) = {
      if (newV < _minV) {
        _minK = Some(t);
        _minV = newV;
      }
      super.updateStatistics(t, oldV, newV);
    }

    override protected[counters] def resetStatistics() {
      _minK = None;
      _minV = Double.MaxValue;
    }

    def min = _minK;
    def argmin = _minV;
  }

  /**
   * Tracks the maximum of the values in the counter
   * @author dramage
   */
  trait Maximum[T] extends TrackedStatistics[T] {
    private var _maxK : Option[T] = None;
    private var _maxV : Double = Double.MinValue;

    override protected[counters] def updateStatistics(t : T, oldV : Double, newV : Double) = {
      if (newV > _maxV) {
        _maxK = Some(t);
        _maxV = newV;
      }
      super.updateStatistics(t, oldV, newV);
    }

    override protected[counters] def resetStatistics() {
      _maxK = None;
      _maxV = Double.MinValue;
    }

    def max = _maxK;
    def argmax = _maxV;
  }

  /**
  * Tracks the log of sum of values in the tracked object.
  * 
  * It turns out to be inefficient to actually track the log total, so we
  * just track whether or not we can keep using it.
  */
  trait LogTotal[T] extends TrackedStatistics[T] {
    private var logTotalOk = true;
    private var logTotal_ = Double.NegativeInfinity;

    def logTotal = if(logTotalOk) {
      logTotal_
    } else {
      val max = valuesIterator.foldLeft(Double.NegativeInfinity)(_ max _);
      logTotal_ = scalanlp.math.Numerics.logSum(valuesIterator,max);
      logTotalOk = true;
      logTotal_
    }

    def valuesIterator: Iterator[Double];

    override protected[counters] def updateStatistics(t: T, oldV: Double, newV: Double) {
      logTotalOk = false;
      super.updateStatistics(t,oldV,newV);
    }
    
    override protected[counters] def resetStatistics() {
      logTotalOk = false;
      super.resetStatistics();
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
  protected val reset  = new ArrayBuffer[()=>Unit]

  /**
  * Called by implementing classes when a new key's value is changed in some way.
  */
  protected[counters] final def updateStatistics(t: T, oldV: Int, newV: Int) {
    statistics foreach ( _.apply(t,oldV,newV) );
  }
  protected[counters] final def resetStatistics() {
    reset foreach (_ apply ());
  }
}

object TrackedIntStatistics {
  /**
  * Tracks the sum of values in the tracked object.
  */
  trait Total[T] extends TrackedIntStatistics[T] {
    def total = total_;

    private var total_ = 0;

    statistics += { (t :T, oldV: Int, newV: Int) =>
      total_ += (newV - oldV);
    }
    reset += { () => total_ = 0}
  }

}
