package scalanlp.math;

import Numerics._;

/**
* Represents a double in log space, to prevent under/overflow
*
* @author dlwh
*/
class LogDouble(val logValue: Double) { 
  def value = Math.exp(logValue);
  def *(other:LogDouble) = new LogDouble(logValue + other.logValue);
  def /(other:LogDouble) = new LogDouble(logValue - other.logValue);
  def +(other:LogDouble) = new LogDouble(logSum(logValue,other.logValue));
  def -(other:LogDouble) = new LogDouble(logDiff(logValue,other.logValue));


  import LogDouble.doubleExtra;
  def *(d : Double) = new LogDouble(logValue + Math.log(d));
  def /(d : Double) = new LogDouble(logValue - Math.log(d));
  def +(d : Double) = new LogDouble(logSum(logValue,Math.log(d)));
  def -(d : Double) = new LogDouble(logDiff(logValue,Math.log(d)));

  override def toString = "LogDouble(" + logValue + ")"
}

object LogDouble {
  implicit def doubleExtra(d : Double) = new {
    def asLogDouble = new LogDouble(d);
    def toLogDouble = new LogDouble(Math.log(d));
    def logValue = Math.log(d);

    // Operations assume the Double is in "Normal" space
    def *(o : LogDouble) = new LogDouble(o.logValue + Math.log(d));
    def /(o : LogDouble) = new LogDouble(Math.log(d) - o.logValue);
    def +(o : LogDouble) = new LogDouble(logSum(o.logValue,Math.log(d)));
    def -(o : LogDouble) = new LogDouble(logDiff(Math.log(d), o.logValue));
  }

  implicit def logDoubleToDouble(d : LogDouble) = d.value;

  def log(d : LogDouble) = this;
  def exp(d : LogDouble) = new LogDouble(d.value);
  def pow(d : LogDouble, p:Double) = new LogDouble(d.logValue * p);
  def pow(d : LogDouble, p:LogDouble) = new LogDouble(d.logValue * p.value);
}
