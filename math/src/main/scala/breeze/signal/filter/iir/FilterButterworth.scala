package breeze.signal.filter.iir

object FilterButterworth {
  
  def design[T](order: OptOrder, omega: OptOmega, tpe: OptFilterType): IIRKernel1D[T] = {
    ???
  }
  
  def order(omega: OptOmega,  attenuationPass: Double, attenuationStop: Double, tpe: OptFilterType): Int = {
		???
  }
}