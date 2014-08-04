package breeze.signal.support

/**
 * @author ktakagaki
 */
import breeze.generic.UFunc
import breeze.macros.expand
import breeze.linalg._
import breeze.signal._
import breeze.signal.OptRange.RangeOpt
import breeze.numerics.isOdd
import breeze.signal.OptRange.RangeOpt
import scala.reflect.ClassTag
import breeze.util.SerializableLogging

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
object CanConvolve extends SerializableLogging {

  @expand
  @expand.valify
  implicit def dvT1DConvolve[@expand.args(Int, Long, Float, Double) T]: CanConvolve[DenseVector[T],DenseVector[T], DenseVector[T]] = {
    new CanConvolve[DenseVector[T],DenseVector[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: DenseVector[T], range: OptRange,
                correlate: Boolean,
                overhang: OptOverhang,
                padding: OptPadding,
                method: OptMethod): DenseVector[T] = {


        //val parsedOptMethod =
        method match {
          case OptMethod.Automatic => require(true)
          case _ => require(false, "currently, only loop convolutions are supported.")
        }

        //ToDo 3: optimize -- padding is not necessary if kernel does not overhang data
        val kl = kernel.length
        val dl = data.length

        //the following will pad data on the left and right, depending upon OptOverhang
        //the padded data will be sent to a full correlation (correlateLoopNoOverhang)
        val paddedData = overhang /*optConvolveOverhangParsed*/ match {

          //No overhang
          case OptOverhang.None => data

          //Overhang as much as possible
          case OptOverhang.Full =>
            DenseVector.vertcat(
              padding match {
                case OptPadding.Cyclical => data( dl - (kl-1) to dl - 1 )
                case OptPadding.Boundary => DenseVector.ones[T](kernel.length-1) * data( 0 )
                case OptPadding.Zero => DenseVector.zeros[T](kernel.length-1)
                case OptPadding.ValueOpt(v: T) => DenseVector.ones[T](kernel.length-1) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              },
              data,
              padding match {
                case OptPadding.Cyclical => data( 0 to kl-1 )
                case OptPadding.Boundary => DenseVector.ones[T](kernel.length-1) * data( dl - 1  )
                case OptPadding.Zero => DenseVector.zeros[T](kernel.length-1)
                case OptPadding.ValueOpt(v: T) => DenseVector.ones[T](kernel.length-1) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              }
            )

          //Overhangs on both sides will sum to kernel.length - 1, thereby giving the same output length as input length
          //Handy for FIR filtering
          case OptOverhang.PreserveLength => {

            val leftPadding: Int =
              if( isOdd(kernel.length) )  (kernel.length -1)/2
              else (kernel.length/2 -1)
            val rightPadding = kernel.length - leftPadding -1

            //Actual padding
            DenseVector.vertcat(
              padding match {
                case OptPadding.Cyclical => data( dl - leftPadding to dl - 1 )
                case OptPadding.Boundary => DenseVector.ones[T](leftPadding /*kernel.length-1*/) * data( 0 )
                case OptPadding.Zero => DenseVector.zeros[T](leftPadding)
                case OptPadding.ValueOpt(v: T) => DenseVector.ones[T](leftPadding) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              },
              data,
              padding match {
                case OptPadding.Cyclical => data( 0 to rightPadding - 1 )
                case OptPadding.Boundary => DenseVector.ones[T](rightPadding) * data( dl - 1  )
                case OptPadding.Zero => DenseVector.zeros[T](rightPadding)
                case OptPadding.ValueOpt(v: T) => DenseVector.ones[T](rightPadding) * v
                case op => require(false, "cannot handle OptPadding value " + op); DenseVector[T]()
              }
            )
          }
          case oc => require(false, "cannot handle OptOverhang value " + oc); data
        }

        val fullOptRangeLength = paddedData.length - kernel.length + 1
        val parsedOptRange = range match {
          case OptRange.All => 0 until fullOptRangeLength
          case RangeOpt( negativeR ) => negativeR.getRangeWithoutNegativeIndexes(fullOptRangeLength)
        }

        //Actual implementation
        if(correlate) correlateLoopNoOverhang( paddedData, kernel, parsedOptRange )
        else correlateLoopNoOverhang( paddedData, reverse(kernel), parsedOptRange )
      }
    }
  }

//  Bad idea, causes ambiguous implicit references when result types are not specified
//  @expand
//  @expand.valify
//  implicit def dvT1DSingleConvolve[@expand.args(Int, Long, Float, Double) T]: CanConvolve[DenseVector[T],DenseVector[T], T] = {
//    new CanConvolve[DenseVector[T],DenseVector[T], T] {
//      def apply(data: DenseVector[T], kernel: DenseVector[T], range: OptRange,
//                correlate: Boolean,
//                overhang: OptOverhang,
//                padding: OptPadding,
//                method: OptMethod): T = {
//        require(overhang == OptOverhang.None, "Overhang must equal OptOverhang.none to return scalar from convolution.")
//        require(data.length == kernel.length, "Data and kernel must have same length to return scalar from convolution. ")
//        if(correlate) sum(data :* kernel) else sum(data :* reverse(kernel))
//      }
//    }
//  }

  @expand
  @expand.valify
  implicit def dvTKernel1DConvolve[@expand.args(Int, Long, Float, Double) T]: CanConvolve[DenseVector[T], FIRKernel1D[T], DenseVector[T]] = {
    new CanConvolve[DenseVector[T], FIRKernel1D[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: FIRKernel1D[T], range: OptRange,
                correlateVal: Boolean,
                overhang: OptOverhang,
                padding: OptPadding,
                method: OptMethod): DenseVector[T] =
        //this is to be expanded to use the fft results within the FIRKernel1D, when using fft convolution
        if(correlateVal) correlate(data, kernel.kernel, range, overhang, padding, method)
        else convolve(data, kernel.kernel, range, overhang, padding, method)
    }
  }



  trait CanCorrelateNoOverhang[Input, KernelType, Output] {
    def apply(data: Input, kernel: KernelType, range: Range): Output
  }

  def correlateLoopNoOverhang[Input, KernelType, Output](data: Input, kernel: KernelType, range: Range)
                                                        (implicit canCorrelateNoOverhang: CanCorrelateNoOverhang[Input, KernelType, Output]): Output =
    canCorrelateNoOverhang(data, kernel, range)


  @expand
  @expand.valify
  implicit def correlateLoopNoOverhangRangeT[@expand.args(Double, Float, Long) T]: CanCorrelateNoOverhang[DenseVector[T], DenseVector[T], DenseVector[T]] =
    new CanCorrelateNoOverhang[DenseVector[T], DenseVector[T], DenseVector[T]] {
      def apply(data: DenseVector[T], kernel: DenseVector[T], range: Range): DenseVector[T] = {
        require( data.length * kernel.length != 0, "data and kernel must be non-empty DenseVectors")
        require( data.length >= kernel.length, "kernel (" + kernel.length + ") cannot be longer than data(" + data.length + ") to be convolved/correlated!")
        require( range.start >= 0 && range.last <= (data.length - kernel.length + 1),
          logger.error(s"range (start ${range.start}, end ${range.end}, step ${range.step}, inclusive ${range.isInclusive}) is OOB for data (length ${data.length}) and kernel (length ${kernel.length})!")
        )

        val dataVect = data.toScalaVector() //make immutable
        val kernelVect = kernel.toScalaVector()
        val tempRange = range.par
        val zero = 0.asInstanceOf[T]

        val tempArr = tempRange.map(
          (count: Int) => {
            var ki: Int = 0
            var sum = zero
            while(ki < kernel.length){
              sum = sum + dataVect( count + ki ) * kernelVect(ki)
              ki = ki + 1
            }
            sum
          }
        ).toArray

        DenseVector(tempArr)
      }
    }

  implicit val correlateLoopNoOverhangRangeInt : CanCorrelateNoOverhang[DenseVector[Int], DenseVector[Int], DenseVector[Int]] =
    new CanCorrelateNoOverhang[DenseVector[Int], DenseVector[Int], DenseVector[Int]] {
      def apply(data: DenseVector[Int], kernel: DenseVector[Int], range: Range): DenseVector[Int] = {
        require( data.length * kernel.length != 0, "data and kernel must be non-empty DenseVectors")
        require( data.length >= kernel.length, "kernel cannot be longer than data to be convolved/corelated!")
        require( range.start >= 0 && range.last <= (data.length - kernel.length + 1),
          logger.error(s"range (start ${range.start}, end ${range.end}, step ${range.step}, inclusive ${range.isInclusive}) is OOB for data (length ${data.length}) and kernel (length ${kernel.length})!")
        )

        val dataL = convert(data, Long).toScalaVector() //make immutable
        val kernelL = convert(kernel, Long).toScalaVector()

        val tempRange = range.par
        val tempArr = tempRange.map(
          (count: Int) => {
            var ki: Int = 0
            var sum = 0L
            while(ki < kernel.length){
              sum = sum + dataL( count + ki ) * kernelL(ki)
              ki = ki + 1
            }
            sum.toInt
          }
        ).toArray
        DenseVector[Int]( tempArr )
//        val tempRangeVect = range.toVector
//        val tempArr = Array[Int](tempRangeVect.length)
//
//        var count = 0
//        while( count < tempRangeVect.length ){
//          var ki: Int = 0
//          var sum: Long = 0L
//          val startTap = tempRangeVect(count)
//          while(ki < kernel.length){
//            sum +=  dataL( startTap + ki ) * kernelL(ki)
//            ki += 1
//          }
//          tempArr(count) = sum.toInt
//          count += 1
//        }
//
//        DenseVector(tempArr)
      }
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
