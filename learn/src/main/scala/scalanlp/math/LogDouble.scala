package scalanlp.math;

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


import Numerics._;

/**
* Represents a double in log space, to prevent under/overflow
* These guys are horribly slow right now, thanks to boxing in Java. Hopefully that will away one day.
*
* @author dlwh
*/
class LogDouble(val logValue: Double) { 
  def value = math.exp(logValue);
  def *(other:LogDouble) = new LogDouble(logValue + other.logValue);
  def /(other:LogDouble) = new LogDouble(logValue - other.logValue);
  def +(other:LogDouble) = new LogDouble(logSum(logValue,other.logValue));
  def -(other:LogDouble) = new LogDouble(logDiff(logValue,other.logValue));


  import LogDouble.doubleExtra;
  def *(d : Double) = new LogDouble(logValue + math.log(d));
  def /(d : Double) = new LogDouble(logValue - math.log(d));
  def +(d : Double) = new LogDouble(logSum(logValue,math.log(d)));
  def -(d : Double) = new LogDouble(logDiff(logValue,math.log(d)));

  override def toString = "LogDouble(" + logValue + ")"
  override def equals(o: Any) = o match {
    case ld: LogDouble => logValue == ld.logValue;
    case _ => false;
  }
  override def hashCode = logValue.hashCode;
}

object LogDouble {
  implicit def doubleExtra(d : Double) = new {
    /**
     * Assumes the double is already logged.
     */
    def asLogDouble = new LogDouble(d);
    
    /**
     * Stores the double in LogSpace
     */
    def toLogDouble = new LogDouble(math.log(d));
    def logValue = math.log(d);

    // Operations assume the Double is in "Normal" space
    def *(o : LogDouble) = new LogDouble(o.logValue + math.log(d));
    def /(o : LogDouble) = new LogDouble(math.log(d) - o.logValue);
    def +(o : LogDouble) = new LogDouble(logSum(o.logValue,math.log(d)));
    def -(o : LogDouble) = new LogDouble(logDiff(math.log(d), o.logValue));
  }

  implicit def logDoubleToDouble(d : LogDouble) = d.value;

  def log(d : LogDouble) = new LogDouble(math.log(d.logValue));
  def exp(d : LogDouble) = new LogDouble(d.value);
  def pow(d : LogDouble, p:Double) = new LogDouble(d.logValue * p);
  def pow(d : LogDouble, p:LogDouble) = new LogDouble(d.logValue * p.value);
}
