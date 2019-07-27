package breeze.linalg.support

object RangeExtender {
  val All = Range(0, -1, 1)
}

class RangeExtender(val re: Range) extends AnyVal {

  def getRangeWithoutNegativeIndexes(totalLength: Int): Range = {
    if (re.isInclusive) {
      val (actualStart: Int, actualEnd: Int) =
        (
          if (re.start < 0) totalLength + re.start else re.start, //actualStart will be given as argument to inclusive range "to"
          if (re.end < 0) totalLength + re.end else re.end //actualEnd will be given as argument to inclusive range "to"
        )
      (actualStart to actualEnd by re.step)

    } else if (re.end < 0 || re.start < 0) {
      throw new IllegalArgumentException(
        "cannot use negative end indexing with 'until', due to ambiguities from Range.end being exclusive")
    } else {
      re
    }
  }

}
