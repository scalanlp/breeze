package breeze.signal.support

/**
 * @author ktakagaki
 */
import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.signal._
import breeze.macros.expand


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
  def apply(data: InputType, kernel: InputType, cyclical: Boolean, optOverhang: OptConvolveOverhang, optConvolveMethod: OptConvolveMethod): OutputType
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
      def apply(data: DenseVector[Double], kernel: DenseVector[Double],
                cyclical: Boolean,
                optOverhang: OptConvolveOverhang,
                optConvolveMethod: OptConvolveMethod): DenseVector[Double] = {

//        optOverhang match  {
//          case x: OptConvolveOverhang.OptDefault => {
//            require(kernel.length <= data.length, "kernel length must be shorter or equal to data")
//            //for(cRes <- 0 until (data.length - kernel.length +1) ) yield sum(kernel :* data(cRes until cRes + kernel.length))
//            val tempRet = new Array[Double](data.length - kernel.length +1)
//            var cKern = 0
//            var cRes = 0
//            while(cRes < tempRet.length ){
//              while(cKern < kernel.length){
//                tempRet(cRes) += kernel(kernel.length - cKern -1) * data(cRes + cKern)
//                cKern += 1
//              }
//              cRes += 1
//              cKern = 0
//            }
//            DenseVector[Double](tempRet)
//          }
//            //What should be returned here for invalid parameters? Throw error?
//          case f => println("The overhang option " + f + " is not supported!"); DenseVector[Double]()
//        }

        DenseVector[Double]()
      }
    }
  }


  @expand
  def convolveLoopNoOverhang[@expand.args(Int, Double, Float, Long) T](data: DenseVector[T], kernel: DenseVector[T]): DenseVector[T] =
    correlateLoopNoOverhang(data, reverse(kernel))
  @expand
  def correlateLoopNoOverhang[@expand.args(Int, Double, Float, Long) T](data: DenseVector[T], kernel: DenseVector[T]): DenseVector[T] = {
    require( data.length * kernel.length != 0, "data and kernel must be non-empty DenseVectors")
    require( data.length >= kernel.length, "kernel cannot be longer than data to be convolved/coorelated!")

    DenseVector.tabulate(data.length - kernel.length + 1)(
      di => {
        var ki: T = 0
        var sum: T = 0
        while(c < kernel.length){
          s += data(di + ki)*kernel(ki)
          c += 1
        }
        s
      }
    )

  }


}

