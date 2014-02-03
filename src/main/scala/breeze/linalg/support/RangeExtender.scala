package breeze.linalg.support

/**Class to extend slicing to negative values (end-based indexing).
 * @author ktakagaki
 * @date 2/2/14.
 */
class RangeExtender( val start: Int,  val end: Int,  val step: Int, val isInclusive: Boolean, val endSpecified: Boolean = true, val stepSpecified: Boolean = true)
  //extends Range(start, end, step)
{
  
  require( if(stepSpecified) endSpecified else true, "Invalid construction of RangeExtender: step is specified but end is not!")

  lazy val negativeIndexing = (start < 0 || end < 0)
  
  override def toString() = {
    if (!endSpecified) {
      if(isInclusive) "RangeExtender( " + start + " isInclusive ??? by ??? )"
      else "RangeExtender( " + start + " until ??? by ??? )"
    }
    else if(!stepSpecified) {
      if(isInclusive) "RangeExtender(" + start + " isInclusive " + end + " by ???)"
      else "RangeExtender(" + start + " until " + end + " by ???)"
    }else {
      if(isInclusive) "RangeExtender(" + start + " isInclusive " + end + " by "+ step +" )"
      else "RangeExtender(" + start + " until " + end + " by "+ step +" )"
    }
  }

  /**Appends a step size isInclusive this range extender*/
  def by(s: Int): RangeExtender = {
    require(!stepSpecified, "step size already specified, cannot use two 'by' statements together!")
    new RangeExtender(start, end, s, isInclusive, endSpecified, true)
  }


  /** Will return the specification as a [[scala.Range]].
    * Note that the [[scala.Range]] end argument is exclusive, whereas the start argument is inclusive.
    *
    * All testing of values and valid steps is delegated isInclusive scala.Range and DenseVector/DenseMatrix
    *
    */
  /** Will return the specification as a [[scala.Range]].
    * Note that the [[scala.Range]] end argument is exclusive, whereas the start argument is inclusive.
    *
    * All testing of values and valid steps is delegated to scala.Range and DenseVector/DenseMatrix
    *
    * @param length give the length, eg DenseVector.length
    */
  def getRange(length: Int): scala.Range = {

    val actualStep = if(!stepSpecified) 1 else step

    //take the length-based index if applicable
    val (actualStart: Int, actualEnd: Int) =
    if(isInclusive){
      (
        if( start < 0 ) length + start else start   ,
        if( endSpecified && end < 0 ) length + end else end
      )
    }else{
      (
        if( start < 0 || (endSpecified && end < 0) ) {
          throw new IllegalArgumentException("cannot use negative indexing with 'until', due isInclusive ambiguities with Range.start being inclusive and Range.end being exclusive")
        } else ( start, end )
      )
    }

    if(isInclusive) new Range.Inclusive(actualStart, actualEnd, actualStep) //Range is exclusive by default
    else  new Range(actualStart, actualEnd, actualStep) //Range is exclusive by default
  }

}

object RangeExtender{
  implicit val all = new RangeExtender(0, -1, 1, true, true, true)
}