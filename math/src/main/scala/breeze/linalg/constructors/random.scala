package breeze.linalg

import breeze.generic.UFunc
import breeze.stats.distributions.Rand

/**Gives a random Double.
  * +  randomDouble()... returns a random double, in [0, 1]
  * +  randomDouble( n: Int )... returns a DenseVector with n random doubles, in [0, 1]
  * +  randomDouble( n: Int, (r1: Double, r2: Double) )... returns a DenseVector with n random doubles, in [r1, r2]
  * +  randomDouble( (n1: Int, n2: Int) )... returns an n1 x n2 DenseMatrix with n random doubles, in [0, 1]
  * +  randomDouble( (n1: Int, n2: Int), (r1: Double, r2: Double) )... returns an n1 x n2 DenseMatrix with n random doubles, in [r1, r2]
  *
 * @author ktakagaki
 * @date 04/30/2014.
 */
object randomDouble extends UFunc {

  lazy val randU = Rand.uniform

  def apply() = randU.get()

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

  implicit object implRandomDouble_2D extends Impl[(Int, Int), DenseMatrix[Double]] {
    def apply( dimensions2: (Int, Int) ): DenseMatrix[Double] = {
      DenseMatrix.tabulate(dimensions2._1, dimensions2._2)((p1: Int, p2: Int) => randU.get())
    }
  }

  implicit object implRandomDouble_2DRange extends Impl2[ (Int, Int), (Double, Double), DenseMatrix[Double]] {
    def apply(dimensions2: (Int, Int), range: (Double, Double) ): DenseMatrix[Double] = {
      require(range._1 <= range._2, "First range value must be smaller than second: " + range.toString() )
      val factor = range._2 - range._1
      DenseMatrix.tabulate(dimensions2._1, dimensions2._2)( (p1: Int, p2: Int) => randU.get()*factor + range._1 )
    }
  }

}

/**Gives a random Int.
  * +  randomInt()... returns a random Int, in [0, 1]
  * +  randomInt( n: Int )... returns a DenseVector with n random Ints, in [0, 1]
  * +  randomInt( n: Int, (r1: Int, r2: Int) )... returns a DenseVector with n random Ints, in [r1, r2)
  * +  randomInt( (n1: Int, n2: Int) )... returns an n1 x n2 DenseMatrix with n random Ints, in [0, 1]
  * +  randomInt( (n1: Int, n2: Int), (r1: Int, r2: Int) )... returns an n1 x n2 DenseMatrix with n random Ints, in [r1, r2)
  *
  * @author ktakagaki
  * @date 04/30/2014.
  */
object randomInt extends UFunc {

  lazy val rand0or1 = Rand.randInt(2)

  def apply(): Int = rand0or1.get()

  implicit val implRandomInt_1D: Impl[Int, DenseVector[Int]] =
    new Impl[Int, DenseVector[Int]] {
      def apply(dimensions1: Int): DenseVector[Int] = {
        DenseVector.tabulate(dimensions1)( p => rand0or1.get() )
      }
    }

  implicit val implRandomInt_1DRange: Impl2[Int, (Int, Int), DenseVector[Int]] =
    new Impl2[Int, (Int, Int), DenseVector[Int]] {
      def apply(dimensions1: Int, range: (Int, Int) ): DenseVector[Int] = {
        require(range._1 <= range._2, "First range value must be smaller than second: " + range.toString() )
        val randInt = Rand.randInt( range._2 - range._1 + 1)
        DenseVector.tabulate(dimensions1)( p => randInt.get() + range._1 )
      }
    }

  implicit val implRandomInt_2D: Impl [(Int, Int), DenseMatrix[Int]] =
    new Impl[ (Int, Int), DenseMatrix[Int]] {
      def apply( dimensions2: (Int, Int) ): DenseMatrix[Int] = {
        DenseMatrix.tabulate(dimensions2._1, dimensions2._2)((p1: Int, p2: Int) => rand0or1.get())
      }
    }

  implicit val implRandomInt_2DRange: Impl2[ (Int, Int), (Int, Int), DenseMatrix[Int]] =
    new Impl2[(Int, Int), (Int, Int), DenseMatrix[Int]] {
      def apply(dimensions2: (Int, Int), range: (Int, Int) ): DenseMatrix[Int] = {
        require(range._1 <= range._2, "First range value must be smaller than second: " + range.toString() )
        val randInt = Rand.randInt(range._2 - range._1 + 1)
        DenseMatrix.tabulate(dimensions2._1, dimensions2._2)( (p1: Int, p2: Int) => randInt.get() + range._1 )
      }
    }

}

/**Alias for [[randomDouble]]
  *
  * @author ktakagaki
  * @date 04/30/2014.
  */
object rand extends UFunc {

  implicit val implRandDouble_1D: Impl[Int, DenseVector[Double]] =
    new Impl[Int, DenseVector[Double]] {
      def apply(dimensions1: Int): DenseVector[Double] = randomDouble(dimensions1)
    }

  implicit val implRandDouble_2D: Impl [(Int, Int), DenseMatrix[Double]] =
    new Impl[ (Int, Int), DenseMatrix[Double]] {
      def apply(dimensions2: (Int, Int)): DenseMatrix[Double] = randomDouble(dimensions2)
    }

}

/**Gives Gaussian-distributed random Double(s)
  * +  randn()... returns a Gaussian random variable with mean 0, variance 1
  * +  randn( n: Int )... returns a DenseVector with n randn's
  * +  randn( (n1: Int, n2: Int) )... returns an n1 x n2 DenseMatrix with randn's
  *
  * @author ktakagaki
  * @date 04/30/2014.
  */
object randn extends UFunc {

  lazy val randnDistr = Rand.gaussian

  def apply(): Double = randnDistr.get()

  implicit val implRandn_1D: Impl[Int, DenseVector[Double]] =
    new Impl[Int, DenseVector[Double]] {
      def apply(dimensions1: Int): DenseVector[Double] = {
        DenseVector.tabulate(dimensions1)( p => randnDistr.get() )
      }
    }


  implicit val implRandn_2D: Impl [(Int, Int), DenseMatrix[Double]] =
    new Impl[ (Int, Int), DenseMatrix[Double]] {
      def apply( dimensions2: (Int, Int) ): DenseMatrix[Double] = {
        DenseMatrix.tabulate(dimensions2._1, dimensions2._2)((p1: Int, p2: Int) => randnDistr.get())
      }
    }


}
