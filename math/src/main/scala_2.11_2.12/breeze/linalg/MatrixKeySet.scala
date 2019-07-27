package breeze.linalg

class MatrixKeySet(rows: Int, cols: Int) extends Set[(Int, Int)] {
  def contains(elem: (Int, Int)): Boolean = elem._1 >= 0 && elem._1 < rows && elem._2 >= 0 && elem._2 < cols

  def +(elem: (Int, Int)): Set[(Int, Int)] = Set() ++ iterator + elem
  def -(elem: (Int, Int)): Set[(Int, Int)] = Set() ++ iterator - elem

  def iterator: Iterator[(Int, Int)] = for { j <- Iterator.range(0, cols); i <- Iterator.range(0, rows) } yield (i, j)

  override def size = rows * cols
}
