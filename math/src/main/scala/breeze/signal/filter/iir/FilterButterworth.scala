package breeze.signal.filter.iir

import breeze.signal.support.IIRKernel1D
import breeze.signal.{OptFilterType, OptOmega, OptOrder}

object FilterButterworth {
  
  def design[Input](order: OptOrder, omega: OptOmega, tpe: OptFilterType): IIRKernel1D[Input] = {
    ???
  }
  
  def order(omega: OptOmega,  attenuationPass: Double, attenuationStop: Double, tpe: OptFilterType): Int = {
		???
  }
}