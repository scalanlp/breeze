package breeze.signal.support

/**
 * @author ktakagaki
 */
import breeze.generic.UFunc
import breeze.macros.expand
import breeze.linalg.{reverse, DenseVector, DenseMatrix}
import breeze.signal._
import breeze.signal.OptRange.RangeOpt
import breeze.numerics.isOdd

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
  def apply(data: Input, kernel: KernelType, range: OptRange,
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

  @expand
  @expand.valify
  implicit def dvT1DConvolve[@expand.args(Int, Long, Float, Double) T]: CanConvolve[DenseVector[T],DenseVector[T], DenseVector[T]] = {
    new CanConvolve[DenseVector[T],DenseVector[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: DenseVector[T], range: OptRange,
                correlate: Boolean,
                overhang: OptOverhang,
                padding: OptPadding,
                method: OptMethod): DenseVector[T] = {

//        val optConvolveOverhangParsed = overhang match {
//          case OptOverhang.None => OptOverhang.Sequence(-1, 1)
//          case OptOverhang.Full => OptOverhang.Sequence(1, -1)
//          case o => o
//        }

        val parsedOptRange = range match {
          case OptRange.All => 0 until data.length
          case RangeOpt( range ) => range
        }

        method match {
          case OptMethod.Automatic => require(true)
          case _ => require(false, "currently, only loop convolutions are supported.")
        }

        //ToDo 3: optimize -- padding is not necessary if kernel does not overhang data
        val kl = kernel.length
        val dl = data.length
        val paddedData = overhang /*optConvolveOverhangParsed*/ match {
          case OptOverhang.None => data
          case OptOverhang.Full =>
            DenseVector.vertcat(
              padding match {
                case OptPadding.Cyclical => data( dl - (kl-1) to dl - 1 )
                case OptPadding.Boundary => DenseVector.ones[T](kernel.length-1) * data( 0 )
                case OptPadding.ValueOpt(v: T) => DenseVector.ones[T](kernel.length-1) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              },
              data,
              padding match {
                case OptPadding.Cyclical => data( 0 to kl-1 )
                case OptPadding.Boundary => DenseVector.ones[T](kernel.length-1) * data( dl - 1  )
                case OptPadding.ValueOpt(v: T) => DenseVector.ones[T](kernel.length-1) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              }
            )
          case OptOverhang.PreserveLength => {
            val leftPadding: Int =
              if( isOdd(kernel.length) )  (kernel.length -1)/2
              else (kernel.length/2 -1)
            val rightPadding = kernel.length - leftPadding
            DenseVector.vertcat(
              padding match {
                case OptPadding.Cyclical => data( dl - leftPadding to dl - 1 )
                case OptPadding.Boundary => DenseVector.ones[T](leftPadding /*kernel.length-1*/) * data( 0 )
                case OptPadding.ValueOpt(v: T) => DenseVector.ones[T](leftPadding) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              },
              data,
              padding match {
                case OptPadding.Cyclical => data( 0 to rightPadding - 1 )
                case OptPadding.Boundary => DenseVector.ones[T](rightPadding) * data( dl - 1  )
                case OptPadding.ValueOpt(v: T) => DenseVector.ones[T](rightPadding) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              }
            )
          }
          case oc => require(false, "cannot handle OptOverhang value " + oc); data
        }

        if(correlate) correlateLoopNoOverhang( paddedData, kernel, parsedOptRange )
        else correlateLoopNoOverhang( paddedData, reverse(kernel), parsedOptRange )
      }
    }
  }

  @expand
  @expand.valify
  implicit def dvTKernel1DConvolve[@expand.args(Int, Long, Float, Double) T]: CanConvolve[DenseVector[T], FIRKernel1D[T], DenseVector[T]] = {
    new CanConvolve[DenseVector[T], FIRKernel1D[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: FIRKernel1D[T], range: OptRange,
                correlate: Boolean,
                overhang: OptOverhang,
                padding: OptPadding,
                method: OptMethod): DenseVector[T] =
        //this is to be expanded to use the fft results within the FIRKernel1D, when using fft convolution
        convolve(data, kernel.kernel, range,
                  overhang,
                  padding,
                  method)
    }
  }



}


object correlateLoopNoOverhang extends UFunc {
  @expand
  @expand.valify
  implicit def correlateLoopNoOverhang[@expand.args(Double, Float, Int, Long) T]: Impl2[DenseVector[T], DenseVector[T], DenseVector[T]] =
    new Impl2[DenseVector[T], DenseVector[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: DenseVector[T]) = correlateLoopNoOverhang(data, kernel, 0 until data.length )
    }

  @expand
  @expand.valify
  implicit def correlateLoopNoOverhangRange[@expand.args(Double, Float, Long) T]: Impl3[DenseVector[T], DenseVector[T], Range, DenseVector[T]] =
    new Impl3[DenseVector[T], DenseVector[T], Range, DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: DenseVector[T], range: Range) = {
        require( data.length * kernel.length != 0, "data and kernel must be non-empty DenseVectors")
        require( data.length >= kernel.length, "kernel cannot be longer than data to be convolved/coorelated!")

        DenseVector.tabulate(range)(
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

  implicit def correlateLoopNoOverhangRangeInt: Impl3[DenseVector[Int], DenseVector[Int], Range, DenseVector[Int]] =
    new Impl3[DenseVector[Int], DenseVector[Int], Range, DenseVector[Int]] {
      def apply(data: DenseVector[Int], kernel: DenseVector[Int], range: Range) = {
        require( data.length * kernel.length != 0, "data and kernel must be non-empty DenseVectors")
        require( data.length >= kernel.length, "kernel cannot be longer than data to be convolved/coorelated!")

        val dataL = data.map(_.toLong)
        val kernelL = kernel.map(_.toLong)
        DenseVector.tabulate(range)(
          di => {
            var ki: Int = 0
            var sum: Long = 0L
            while(ki < kernel.length){
              sum += dataL( (di + ki) ) * kernelL(ki)
              ki += 1
            }
            sum.toInt
          }
        )
      }
    }

//  /**FFT-based FIR filtering using overlap-add method.
//    *
//    * @param filter
//    * @param data data to be filtered
//    */
//  public static double[] filterFFT(double[] filter, double[] data){
//    int dataL  = data.length;
//    int filterL  = filter.length;
//    //    ## Use FFT with the smallest power of 2 which is >= length (x) +
//    //    ## length (b) - 1 as number of points ...
//    int N = K.nextpow2( dataL + filterL - 1 );
//
//    return filterFFTImpl(KKJTransforms.fft(filter, N), data, N, filterL, dataL);
//  }
//  //    private static double[] filterFFTImpl(Complex[] filterFFT, double[] data, int N, int filterL, int dataL){
//  //        double[] y =
//  //                K.real(
//  //                    K.ifft(
//  //                        K.multiply(K.fft(data, N), filterFFT)
//  //                    )
//  //                );
//  ////        return K.copy(y,K.r(0,dataL + filterL - 1));
//  ////        return y;
//  //        return K.copy(y,K.r(filterL/2,filterL/2+dataL-1));
//  //    }
//  private static double[] filterFFTImpl(ComplexArray filterFFT, double[] data, int N, int filterL, int dataL){
//    ComplexArray tempret=KKJTransforms.fft(data, N);
//    tempret.multiply(filterFFT);
//    double[] tempretD = KKJTransforms.ifft(tempret).getRealArray();
//    //        return K.copy(y,K.r(0,dataL + filterL - 1));
//    //        return y;
//    return K.copy(tempretD,K.r(filterL/2,filterL/2+dataL-1));
//  }
//
//
//  public static double[] filterFFT(double[] filter, double[] data, int N){
//    int dataL  = data.length;
//    int filterL  = filter.length;
//    //    ## Use overlap-add method ...
//    int n = K.nextpow2( K.max(N, filterL) );
//    //K.pow(2, K.ceil( K.log( K.max(N, l_b)) / K.log(2)));
//    int L = n - filterL + 1;
//    ComplexArray B = KKJTransforms.fft(filter, n);
//    //Complex[] B = K.fft(filter, n);
//    int R = (int)K.ceil(dataL / L);
//    double[] y = K.tableC(0d, dataL);//zeros1D(dataL);
//
//    for(int r=1; r<=R; r++){
//      int lo  = (r - 1) * L;// + 1;
//      int hi  = K.min(r * L, dataL) -1;
//      ComplexArray tempfft = KKJTransforms.fft(K.copy(data, K.r(lo, hi)), n);
//      tempfft.multiply(B);
//      double[] tmp = KKJTransforms.ifft( tempfft ).getRealArray();
//      //            double[] tmp = K.real(
//      //                    K.ifft( K.multiply( K.fft( K.copy(data, K.r(lo, hi)), n), B)  )
//      //                    );
//      hi  = K.min(lo+N-1, dataL - 1);
//      //ToDo: what was this!!! putTo/putTo            K.putTo(y, K.r(lo, hi),
//      K.putTo(y, K.r(lo, hi),
//        K.add(
//          K.copy(y, K.r(lo, hi)),
//          K.copy(tmp, K.r(0, hi-lo))
//        )
//      );
//      //y(lo:hi) = y (lo:hi) + tmp (1:(hi-lo+1));
//    }
//    return y;
//
//    //y = reshape (y (1:l_x), r_x, c_x);
//
//    //  ## Final cleanups:  if both x and b are real respectively integer, y
//    //  ## should also be
//
//    //          if (! (any (imag (x)) || any (imag (b))))
//    //            y = real (y);
//    //            endif
//    //        if (! (any (x - round (x)) || any (b - round (b))))
//    //            y = round (y);
//    //        endif
//
//  }
//  //</editor-fold>

}