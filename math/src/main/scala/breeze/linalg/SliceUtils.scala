package breeze.linalg

import scala.collection.immutable.IndexedSeq

/**
 * Utility class that handles negative row/column indicies and OOB checking
 *
 * @author Michael Petnuch
 */
private object SliceUtils {
  def mapColumnSeq[V](cols: Seq[Int], nCols: Int) = cols match {
    case range: Range => range.getRangeWithoutNegativeIndexes(nCols).map(col => mapColumn(col, nCols))
    case _ => cols.map(col => mapColumn(col, nCols)).toIndexedSeq
  }

  def mapColumn[V](col: Int, nCols: Int): Int = col match {
    case oob if col < -nCols => throw new ArrayIndexOutOfBoundsException("Column must be in bounds for slice!")
    case oob if col >= nCols => throw new ArrayIndexOutOfBoundsException("Column must be in bounds for slice!")
    case neg if col < 0 => neg + nCols
    case _ => col
  }

  def mapRowSeq[V](rows: Seq[Int], nRows: Int): IndexedSeq[Int] = rows match {
    case range: Range => range.getRangeWithoutNegativeIndexes(nRows).map(row => mapRow(row, nRows))
    case _ => rows.map(row => mapRow(row, nRows)).toIndexedSeq
  }

  def mapRow[V](row: Int, nRows: Int): Int = row match {
    case oob if row < -nRows => throw new ArrayIndexOutOfBoundsException("Row must be in bounds for slice!")
    case oob if row >= nRows => throw new ArrayIndexOutOfBoundsException("Row must be in bounds for slice!")
    case neg if row < 0 => neg + nRows
    case _ => row
  }
}
