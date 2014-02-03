package breeze.linalg.support

/**Class to extend slicing to negative values (end-based indexing).
 * @author ktakagaki
 * @date 2/2/14.
 */
class RangeExtender(val start: Int, val end: Int, val step: Int, val endSpecified: Boolean = true, val stepSpecified: Boolean = true) {

  require( if(stepSpecified) endSpecified else true, "Invalid construction of RangeExtender: step is specified but end is not!")

  /**This constructor will be called first by [[breeze.linalg.RichIntMethods.apply]].*/
  def this(start: Int) {
    this(start, 0, 0, false, false)
  }
  /**This constructor will be called second by [[breeze.linalg.RichIntMethods.apply]].*/
  def this(start: Int, end: Int) {
    this(start, end, 0, true, false)
  }


  override def toString = {
    if(!endSpecified) "RangeExtender(" + start + ", ???, ???)"
    else if(!stepSpecified) "RangeExtender(" + start + ", " + end + ", ???)"
    else throw new IllegalArgumentException("only 3 values are allowed in a range,  start :: end :: step")
  }

  def ##(x: Int): RangeExtender = {
    if(!endSpecified) new RangeExtender(start)
    else if(!stepSpecified) new RangeExtender(start, end, x)
    else throw new IllegalArgumentException("only 3 values are allowed in a range,  start :: end :: step")
  }


  /** Will return the specification as a [[scala.Range]].
    * Note that [[breeze.linalg.support.RangeExtender.end]] is inclusive (specifies the ending index),
    * whereas the [[scala.Range]] end argument is exclusive.
    *
    * All testing of values and valid steps is delegated to scala.Range and DenseVector/DenseMatrix
    *
    */
  def getRange(length: Int): scala.Range = {

    val actualStep = if(!stepSpecified) 1 else step
    val actualStart = if( start < 0 ) length + start else start
    val actualEnd = if( endSpecified && end < 0 ) length + end else end

    if(endSpecified) new Range.Inclusive(actualStart, actualEnd, actualStep) //Range is exclusive by default
    else  new Range.Inclusive(actualStart, length, actualStep)
  }

}
