package breeze.linalg

import breeze.generic.UFunc
import breeze.stats.distributions.{RandBasis, Rand}
import scala.reflect.ClassTag
import breeze.storage.Zero
import breeze.macros._

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
object randomDouble extends RandomGeneratorUFunc[Double] {
  protected def gen(implicit basis: RandBasis): Rand[Double] = basis.uniform
  protected def genRange(low: Double, high: Double)(implicit basis: RandBasis): Rand[Double] = {
    require(high >= low, s"High term must be greater than low term. ($low, $high)")
    val range = (high - low)
    basis.uniform.map(_ * range + low)
  }

  protected val _classTag: ClassTag[Double] = scala.reflect.classTag[Double]
  protected val _zero = Zero[Double](0.0)

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
object randomInt extends RandomGeneratorUFunc[Int] {

  protected def gen(implicit basis: RandBasis): Rand[Int] = genRange(0, 1)
  protected def genRange(low: Int, high: Int)(implicit basis: RandBasis): Rand[Int] = {
    require(high >= low, s"High term must be greater than low term. ($low, $high)")
    basis.randInt(high - low + 1).map(_ + low)
  }

  protected val _classTag: ClassTag[Int] = scala.reflect.classTag[Int]
  protected val _zero = Zero[Int](0)

}

/**Gives Gaussian-distributed random Double(s)
 * +  randn()... returns a Gaussian random variable with mean 0, variance 1
 * +  randn( n: Int )... returns a DenseVector with n randn's
 * +  randn( (n1: Int, n2: Int) )... returns an n1 x n2 DenseMatrix with randn's
 *
 * @author ktakagaki
 * @date 04/30/2014.
 */
object randn extends RandomGeneratorUFunc[Double] {

  protected def gen(implicit basis: RandBasis): Rand[Double] = basis.gaussian
  protected def genRange(low: Double, high: Double)(implicit basis: RandBasis): Rand[Double] =
    basis.gaussian(low, high)

  protected val _classTag: ClassTag[Double] = scala.reflect.classTag[Double]
  protected val _zero = Zero.DoubleZero

}

trait RandomGeneratorUFunc[T] extends UFunc {
  protected def gen(implicit basis: RandBasis): Rand[T]
  protected def genRange(low: T, high: T)(implicit basis: RandBasis): Rand[T]
  protected implicit val _classTag: ClassTag[T]
  protected implicit val _zero: Zero[T]

  def apply()(implicit basis: RandBasis) = gen(basis).draw()

  implicit def implRandomT_1D(implicit basis: RandBasis): Impl[Int, DenseVector[T]] =
    new Impl[Int, DenseVector[T]] {
      def apply(dimensions1: Int): DenseVector[T] = DenseVector.rand(dimensions1, gen)
    }

  implicit def implRandomT_1DRange(implicit basis: RandBasis): Impl2[Int, (T, T), DenseVector[T]] =
    new Impl2[Int, (T, T), DenseVector[T]] {
      def apply(dimensions1: Int, range: (T, T)): DenseVector[T] = {
        DenseVector.rand(dimensions1, genRange(range._1, range._2))
      }
    }

  implicit def implRandomT_2D(implicit basis: RandBasis): Impl[(Int, Int), DenseMatrix[T]] =
    new Impl[(Int, Int), DenseMatrix[T]] {
      def apply(dimensions2: (Int, Int)): DenseMatrix[T] = {
        DenseMatrix.rand(dimensions2._1, dimensions2._2, gen)
      }
    }

  implicit def implRandomT_2DRange(implicit basis: RandBasis): Impl2[(Int, Int), (T, T), DenseMatrix[T]] =
    new Impl2[(Int, Int), (T, T), DenseMatrix[T]] {
      def apply(dimensions2: (Int, Int), range: (T, T)): DenseMatrix[T] = {
        DenseMatrix.rand(dimensions2._1, dimensions2._2, genRange(range._1, range._2))
      }
    }

}
