package breeze.signal.filter.fir

import breeze.linalg.DenseVector
import breeze.signal.support.{CanFirwin, FIRKernel1D}
import breeze.signal._

object FilterFirwin {

  def design[Input](taps: Int, omega: OptOmega, samplingRate: Double = 2d,
                zeroPass: Boolean = true,
                scale: Boolean = true, multiplier: Double = 1d,
                optWindow: OptWindowFunction = OptWindowFunction.Hamming()  )
               (implicit canFirwin: CanFirwin[Input]): FIRKernel1D[Input] = ???
//    omega match {
//
//                  case o:OptOmega.DoubleValue => canFirwin(taps, DenseVector(o.omega), samplingRate, zeroPass, scale, multiplier, optWindow)
//                  case o:OptOmega.TupleValue => canFirwin(taps, DenseVector(o.omega1, o.omega2), samplingRate, zeroPass, scale, multiplier, optWindow)
//  }
   
}