package breeze.linalg.functions

import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.Options.OptPadMode
import breeze.linalg.{DenseVector, Window, Windowed, WindowedVector}

object window extends UFunc with MappingUFunc {
  implicit def standardWindowDenseVectorImpl[T] : Impl2[DenseVector[T], Int, WindowedVector[DenseVector[T], DenseVector[T]]] = {
    new Impl2[DenseVector[T], Int, WindowedVector[DenseVector[T], DenseVector[T]]] {
      def apply(from: DenseVector[T], length: Int)  : WindowedVector[DenseVector[T], DenseVector[T]] = WindowedVector[T](from, length)
    }
  }

  implicit def paddedWindowDenseVectorImpl[T] : Impl3[DenseVector[T], Int, OptPadMode, WindowedVector[DenseVector[T], DenseVector[T]]] = {
    new Impl3[DenseVector[T], Int, OptPadMode, WindowedVector[DenseVector[T], DenseVector[T]]] {
      def apply(from: DenseVector[T], length: Int, padMode: OptPadMode) : WindowedVector[DenseVector[T], DenseVector[T]] = WindowedVector(from, length, padMode)
    }
  }

  implicit def windowDenseVectorImpl[T] : Impl2[DenseVector[T], Window, Windowed[DenseVector[T], DenseVector[T]]] = {
    new Impl2[DenseVector[T], Window, WindowedVector[DenseVector[T], DenseVector[T]]] {
      def apply(from: DenseVector[T], window: Window)  : WindowedVector[DenseVector[T], DenseVector[T]] = WindowedVector(from, window)
    }
  }
}