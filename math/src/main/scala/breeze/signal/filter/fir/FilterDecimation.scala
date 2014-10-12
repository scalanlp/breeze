package breeze.signal.filter

import breeze.linalg.DenseVector
import breeze.signal.filter.support.FIRKernel1D

object FilterDecimation {

  def apply[V]( order: OptOrder, decimationFactor: Int )( data: DenseVector[V] ): DenseVector[V] = ???

  def design[Input](order: OptOrder, decimationFactor: Int): FIRKernel1D[Input] = {
    ???
  }
}

