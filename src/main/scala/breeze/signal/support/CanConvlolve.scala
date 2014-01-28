package breeze.signal.support

/**
 * @author ktakagaki
 */
import breeze.generic.UFunc
import breeze.macros.expand
import breeze.linalg.{reverse, DenseVector, DenseMatrix}
import breeze.signal._

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
trait CanConvolve[Input, KernelType, Output] {
  def apply(data: Input, kernel: KernelType,
            correlate: Boolean,
            overhang: OptOverhang,
            padding: OptPadding,
            method: OptMethod): Output
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

  /** Use via implicit delegate syntax convolve(data: DenseVector, kernel)
    *
    */
  @expand
  @expand.valify
  implicit def dvT1DConvolve[@expand.args(Int, Long, Float, Double) T]: CanConvolve[DenseVector[T],DenseVector[T], DenseVector[T]] = {
    new CanConvolve[DenseVector[T],DenseVector[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: DenseVector[T],
                correlate: Boolean,
                overhang: OptOverhang,
                padding: OptPadding,
                method: OptMethod): DenseVector[T] = {

        val optConvolveOverhangParsed = overhang match {
          case OptOverhang.None => OptOverhang.Sequence(-1, 1)
          case OptOverhang.Full => OptOverhang.Sequence(1, -1)
          case o => o
        }

        method match {
          case OptMethod.Automatic => require(true)
          case _ => require(false, "currently, only loop convolutions are supported.")
        }

        val kl = kernel.length
        val dl = data.length
        val paddedData = optConvolveOverhangParsed match {
          case OptOverhang.Sequence(-1, 1) => data
          case OptOverhang.Sequence(1, -1) =>
            DenseVector.vertcat(
              padding match {
                case OptPadding.Cyclical => data( dl - (kl-1) to dl - 1 )
                case OptPadding.Boundary => DenseVector.ones[T](kernel.length-1) * data( 0 )
                case OptPadding.Value(v: T) => DenseVector.ones[T](kernel.length-1) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              },
              data,
              padding match {
                case OptPadding.Cyclical => data( 0 to kl-1 )
                case OptPadding.Boundary => DenseVector.ones[T](kernel.length-1) * data( dl - 1  )
                case OptPadding.Value(v: T) => DenseVector.ones[T](kernel.length-1) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              }
            )
          case oc => require(false, "cannot handle OptOverhang value " + oc); data
        }

        if(correlate) correlateLoopNoOverhang( paddedData, kernel )
        else correlateLoopNoOverhang( paddedData, reverse(kernel) )
      }
    }
  }

  /** Use via implicit delegate syntax convolve(data: DenseVector, kernel)
    *
    */
  @expand
  @expand.valify
  implicit def dvTKernel1DConvolve[@expand.args(Int, Long, Float, Double) T]: CanConvolve[DenseVector[T], FIRKernel1D[T], DenseVector[T]] = {
    new CanConvolve[DenseVector[T], FIRKernel1D[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: FIRKernel1D[T],
                correlate: Boolean,
                overhang: OptOverhang,
                padding: OptPadding,
                method: OptMethod): DenseVector[T] =
        //this is to be expanded to use the fft results within the FIRKernel1D, when using fft convolution
        convolve(data, kernel.kernel, overhang, padding, method)
    }
  }


}


object correlateLoopNoOverhang extends UFunc{
  @expand
  @expand.valify
  implicit def correlateLoopNoOverhang[@expand.args(Int, Double, Float, Long) T]: Impl2[DenseVector[T], DenseVector[T], DenseVector[T]] =
    new Impl2[DenseVector[T], DenseVector[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: DenseVector[T]) = {
        require( data.length * kernel.length != 0, "data and kernel must be non-empty DenseVectors")
        require( data.length >= kernel.length, "kernel cannot be longer than data to be convolved/coorelated!")

        DenseVector.tabulate(data.length - kernel.length + 1)(
          di => {
            var ki: Int = 0
            var sum: T = 0
            while(ki < kernel.length){
              sum += data( (di + ki) ) * kernel(ki)
              ki += 1
            }
            sum
          }
        )

      }
    }

}