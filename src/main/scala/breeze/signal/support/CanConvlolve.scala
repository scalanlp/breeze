package breeze.signal.support

/**
 * @author ktakagaki
 */
import breeze.linalg.{DenseVector, DenseMatrix}


//ToDo 1: provide convolve of Integer and other DenseVectors
//ToDo 1: provide convolve of DenseMatrix
//ToDo 2: program fft convolution as option

/**
 * Construction delegate trait for convolving type InputType.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
 *
 * @author ktakagaki
 */
trait CanConvolve[InputType, OutputType] {
  def apply(kernel: InputType, data: InputType, overhangOpt: OptOverhang): OutputType
}

/**
 * Construction delegate for convolving type InputType.</p>
 * Implementation details (especially
 * option arguments) may be added in the future, so it is recommended not
 * to call these implicit delegates directly. Instead, use convolve(x: DenseVector).
 *
 * @author ktakagaki
 */
object CanConvolve {

  /** Use via implicit delegate syntax fft(x: DenseVector)
    *
    */
  implicit val dvDouble1DConvolve : CanConvolve[DenseVector[Double], DenseVector[Double]] = {
    new CanConvolve[DenseVector[Double], DenseVector[Double]] {
      def apply(kernel: DenseVector[Double], data: DenseVector[Double], optOverhang: OptOverhang) = {
        optOverhang match  {
          case x: OptOverhang.Default => {
            require(kernel.length <= data.length, "kernel length must be shorter or equal to data")
            //for(cRes <- 0 until (data.length - kernel.length +1) ) yield sum(kernel :* data(cRes until cRes + kernel.length))
            val tempRet = new Array[Double](data.length - kernel.length +1)
            var cKern = 0
            var cRes = 0
            while(cRes < tempRet.length ){
              while(cKern < kernel.length){
                tempRet(cRes) += kernel(kernel.length - cKern -1) * data(cRes + cKern)
                cKern += 1
              }
              cRes += 1
              cKern = 0
            }
            DenseVector[Double](tempRet)
          }
            //What should be returned here for invalid parameters? Throw error?
          case f => println("The overhang option " + f + " is not supported!"); DenseVector[Double]()
        }

      }
    }
  }




}

abstract class OptOverhang
object OptOverhang{
  case class Default() extends OptOverhang
  case class Sequence(k0: Int, k1: Int) extends OptOverhang
  case class Integer(k: Int) extends OptOverhang
}

abstract class OptPadding
object OptPadding{
  case class None() extends OptPadding
  case class Value[T](value: T) extends OptPadding
}
