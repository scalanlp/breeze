package breeze.signal

import breeze.signal._
import breeze.signal.support._
import breeze.linalg.DenseVector
import breeze.numerics.isEven
import breeze.macros.expand

package object filter {
  
  // implicit conversion for options
  implicit def intToOptOrder(n: Int): OptOrder = new OptOrder.IntValue(n)
  // implicit conversion for filter cuttoffs
  // If sampling rate is the default Nyqist frequency (i.e. samplingRate == 2),
  // values must be in (0,1)
  implicit def doubleToOptOmega(omega: Double): OptOmega = new OptOmega.Scalar(omega)
  //can be specified as a Tuple of Doubles for band-stop or band-pass filters
  //(i.e. filters with 2 cutoff frequencies
  implicit def tuple2ToOptOmega(omega: Tuple2[Double, Double]):OptOmega = new OptOmega.Tuple(omega._1, omega._2)
  
  // design shorthands for filter design
  def designFiltButterworth[T](order: OptOrder, omega: OptOmega, tpe: OptFilterType) = iir.FilterButterworth.design[T](order, omega, tpe)
  
  
  // <editor-fold desc="filter">
  /** Filter input data with the specified kernel and options.
    *
    * @param data data to be filtered
    * @param kernel filter kernel
    * @param overhang  whether to have overhanging values - defaults to no overhang. See [[breeze.signal.OptOverhang]]
    * @param padding  how to pad the values - defaults to no padding. See [[breeze.signal.OptPadding]]
    * @param canFilter  (implicit delegate to perform filtering on specific Input data types)
    * @return
    */
  def filter[Input, Kernel, Output](data: Input, kernel: Kernel,
                            overhang: OptOverhang = OptOverhang.PreserveLength,
                            padding: OptPadding = OptPadding.Zero)
        (implicit canFilter: CanFilter[Input, Kernel, Output]): Output = canFilter(data, kernel, overhang, padding)
  // </editor-fold>
}
