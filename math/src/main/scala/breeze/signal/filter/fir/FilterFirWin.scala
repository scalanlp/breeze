package breeze.signal.filter.fir

object FilterFirWin {
  def design[T](order: OptOrder, omega: OptOmega, window: OptWindow, tpe: OptFilterType, samplingRate: Double = 2d): FIRKernel1D[T] = {
    ???
  }
  
  def design[T](taps: Int, omega: OptOmega, samplingRate: Double = 2d,
                zeroPass: Boolean = true,
                scale: Boolean = true, multiplier: Double = 1d,
                optWindow: OptWindowFunction = OptWindowFunction.Hamming()  )
               (implicit canFirwin: CanFirwin[T]): FIRKernel1D[T] = canFirwin(taps, omegas, samplingRate, zeroPass, scale, multiplier, optWindow)
   
}