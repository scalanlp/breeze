package breeze.signal.filter

import breeze.signal.filter.support.IIRKernel1D

object FilterButterworth {
  
  def design[Input](order: OptOrder, omega: OptOmega, tpe: OptFilterTpe): IIRKernel1D[Input] = {
    ???
  }
  
  def order(omega: OptOmega,  attenuationPass: Double, attenuationStop: Double, tpe: OptFilterTpe): Int = {
		???
  }
}