package breeze.linalg

import breeze.generic.UFunc
import breeze.stats.distributions.Rand

/**
 * @author ktakagaki
 * @date 04/30/2014.
 */
object randomDouble extends UFunc {

  lazy val randU = Rand.uniform

  implicit object implRandomDouble_1D extends Impl[Int, DenseVector[Double]] {
    def apply(dimensions1: Int): DenseVector[Double] = DenseVector.tabulate(dimensions1)(p => randU.get())
  }

  implicit object implRandomDouble_1DRange extends Impl2[Int, (Double, Double), DenseVector[Double]] {
    def apply(dimensions1: Int, range: (Double, Double) ): DenseVector[Double] = {
      require(range._1 <= range._2, "First range value must be smaller than second: " + range.toString() )
      val factor = range._2 - range._1
      DenseVector.tabulate[Double](dimensions1)( p => randU.get()*factor + range._1 )
    }
  }

//  implicit val implRandomDouble_2D: Impl[(Int, Int), DenseMatrix[Double]] =
//    new Impl[ (Int, Int), DenseMatrix[Double]] {
//      def apply( dimensions2: (Int, Int) ): DenseMatrix[Double] = {
//        val randU = Rand.uniform
//        DenseMatrix.tabulate(dimensions2._1, dimensions2._2)((p1: Int, p2: Int) => randU.get())
//      }
//    }

//  implicit val implRandomDouble_2DRange: Impl2[ (Int, Int), (Double, Double), DenseMatrix[Double]] =
//    new Impl2[(Int, Int), (Double, Double), DenseMatrix[Double]] {
//      def apply(dimensions2: (Int, Int), range: (Double, Double) ): DenseMatrix[Double] = {
//        require(range._1 <= range._2, "First range value must be smaller than second: " + range.toString() )
//        val randU = Rand.uniform
//        val factor = range._2 - range._1
//        DenseMatrix.tabulate(dimensions2._1, dimensions2._2)( (p1: Int, p2: Int) => randU.get()*factor + range._1 )
//      }
//    }

}

//object randomInt extends UFunc {
//
//  implicit val implRandomInt_1D: Impl[Int, DenseVector[Int]] =
//    new Impl[Int, DenseVector[Int]] {
//      def apply(dimensions1: Int): DenseVector[Int] = {
//        val randInt = Rand.randInt(2)
//        DenseVector.tabulate(dimensions1)( p => randInt.get() )
//      }
//    }
//
//  implicit val implRandomInt_1DRange: Impl2[Int, (Int, Int), DenseVector[Int]] =
//    new Impl2[Int, (Int, Int), DenseVector[Int]] {
//      def apply(dimensions1: Int, range: (Int, Int) ): DenseVector[Int] = {
//        require(range._1 <= range._2, "First range value must be smaller than second: " + range.toString() )
//        val randInt = Rand.randInt( range._2 - range._1 + 1)
//        DenseVector.tabulate(dimensions1)( p => randInt.get() + range._1 )
//      }
//    }
//
//  implicit val implRandomInt_2D: Impl [(Int, Int), DenseMatrix[Int]] =
//    new Impl[ (Int, Int), DenseMatrix[Int]] {
//      def apply( dimensions2: (Int, Int) ): DenseMatrix[Int] = {
//        val randInt = Rand.randInt(2)
//        DenseMatrix.tabulate(dimensions2._1, dimensions2._2)((p1: Int, p2: Int) => randInt.get())
//      }
//    }
//
//  implicit val implRandomInt_2DRange: Impl2[ (Int, Int), (Int, Int), DenseMatrix[Int]] =
//    new Impl2[(Int, Int), (Int, Int), DenseMatrix[Int]] {
//      def apply(dimensions2: (Int, Int), range: (Int, Int) ): DenseMatrix[Int] = {
//        require(range._1 <= range._2, "First range value must be smaller than second: " + range.toString() )
//        val randInt = Rand.randInt(range._2 - range._1 + 1)
//        DenseMatrix.tabulate(dimensions2._1, dimensions2._2)( (p1: Int, p2: Int) => randInt.get() + range._1 )
//      }
//    }
//
//}

//object rand extends UFunc {
//
//  implicit val implRandDouble_1D: Impl[Int, DenseVector[Double]] =
//    new Impl[Int, DenseVector[Double]] {
//      def apply(dimensions1: Int): DenseVector[Double] = randomDouble(dimensions1)
//    }
//
//  implicit val implRandDouble_2D: Impl [(Int, Int), DenseMatrix[Double]] =
//    new Impl[ (Int, Int), DenseMatrix[Double]] {
//      def apply(dimensions2: (Int, Int)): DenseMatrix[Double] = randomDouble(dimensions2)
//    }
//
//}
