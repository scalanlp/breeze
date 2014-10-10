package breeze.signal.filter.fir

import breeze.linalg.DenseVector
import breeze.signal.OptOrder
import breeze.signal.support.FIRKernel1D

object FilterDecimation {

  def apply[V]( order: OptOrder, decimationFactor: Int )( data: DenseVector[V] ): DenseVector[V] = ???

  def design[Input](order: OptOrder, decimationFactor: Int): FIRKernel1D[Input] = {
    ???
  }
}

