package breeze.linalg

/**Immutable class to represent matrix dimension sizes
 * Created by ktakagaki on 15/04/11.
 */
case class Dimensions {

  private var length = 0
  final def getLength() = length

  private var dimensions = Vector[Int]()
  final def getDimensions() = dimensions

  def Dimensions(dim1: Int) {
    val length = 1
    dimensions = Vector(dim1)
  }
  def Dimensions(dim1: Int, dim2: Int) {
    val length = 2
    dimensions = Vector(dim1, dim2)
  }
  def Dimensions(dim1: Int, dim2: Int, dim3: Int) {
    val length = 3
    dimensions = Vector(dim1, dim2, dim3)
  }


}

