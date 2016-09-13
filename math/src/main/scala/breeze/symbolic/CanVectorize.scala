package breeze.symbolic

import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.DenseVector
import shapeless.labelled.FieldType
import shapeless.ops.hlist.IsHCons
import shapeless.ops.record.Keys
import shapeless.{::, HList, HNil, LUBConstraint, Const => _}

import scala.reflect.ClassTag

/**
  * Type-class to lift a sequence of scalar SymbolicFunctions to a vectorized representation
  * @tparam E The type of the scalar SymbolicFunction
  */
trait CanVectorize[-E] {
  type V
  def vectorize(fs: Seq[E]): V
}

abstract class AbstractCanVectorize[-E](val name: String) extends CanVectorize[E] {
  override def toString: String = name
}

trait LowestPriorityCanVectorize {
  /*
  implicit def wrapSymbolicFunctions[F <: SymbolicFunction[F]]: CanVectorize.Aux[F, VectorizedSymbolicFunction[F]] =
    new AbstractCanVectorize[F]("F") {
      type V = VectorizedSymbolicFunction[F]
      override def vectorize(fs: Seq[F]): VectorizedSymbolicFunction[F] = {
        println(s"Couldn't vectorize $fs to anything better than VectorizedSymbolicFunction")
        VectorizedSymbolicFunction(fs)
      }
    }
    */
}

trait LowPriorityCanVectorize extends LowestPriorityCanVectorize {

  implicit def canVectorizeConst[T: ClassTag]: CanVectorize.Aux[Const[T], Const[DenseVector[T]]] =
    new AbstractCanVectorize[Const[T]]("Const") {
      type V = Const[DenseVector[T]]
      def vectorize(fs: Seq[Const[T]]): V = Const(DenseVector(fs.map(_.const): _*))
    }

  implicit def canVectorizeConstZero: CanVectorize.Aux[ConstZero, ConstZero] =
    new AbstractCanVectorize[ConstZero]("ConstZero") {
      type V = ConstZero
      def vectorize(fs: Seq[ConstZero]): V = ConstZero()
    }

  implicit def canVectorizeConstOne: CanVectorize.Aux[ConstOne, ConstOne] =
    new AbstractCanVectorize[ConstOne]("ConstOne") {
      type V = ConstOne
      def vectorize(fs: Seq[ConstOne]): V = ConstOne()
    }

  implicit def canVectorizeVar: CanVectorize.Aux[Var, Var] =
    new AbstractCanVectorize[Var]("Var") {
      type V = Var
      def vectorize(fs: Seq[Var]): V = Var()
    }

  implicit def canVectorizeHNil: CanVectorize.Aux[HNil, HNil] =
    new AbstractCanVectorize[HNil]("HNil") {
      type V = HNil
      def vectorize(fs: Seq[HNil]): V = HNil
    }

  implicit def canVectorizeHList[F, L <: HList, T <: HList](
    implicit canVectorizeHead: CanVectorize[F],
    canVectorizeTail: CanVectorize.Aux[L, T]
  ): CanVectorize.Aux[F :: L, canVectorizeHead.V :: T] =
    new AbstractCanVectorize[F :: L]("F :: L") {
      type V = canVectorizeHead.V :: T
      def vectorize(fs: Seq[F :: L]): V = canVectorizeHead.vectorize(fs.map(_.head)) :: canVectorizeTail.vectorize(fs.map(_.tail))
    }

  implicit def canVectorizeExponential[F1 <: SymbolicFunction[F1], LF1 <: SymbolicFunction[LF1]](
    implicit canVectorizeF1: CanVectorize.Aux[F1, LF1]
  ): CanVectorize.Aux[Exponential[F1], Exponential[LF1]] =
    new AbstractCanVectorize[Exponential[F1]]("Exponential[F1]") {
      type V = Exponential[LF1]
      def vectorize(fs: Seq[Exponential[F1]]) = Exponential(canVectorizeF1.vectorize(fs.map(_.fn)))
    }

  implicit def canVectorizeLogarithm[F1 <: SymbolicFunction[F1], LF1 <: SymbolicFunction[LF1]](
    implicit canVectorizeF1: CanVectorize.Aux[F1, LF1]
  ): CanVectorize.Aux[Logarithm[F1], Logarithm[LF1]] =
    new AbstractCanVectorize[Logarithm[F1]]("Logarithm[F1]") {
      type V = Logarithm[LF1]
      def vectorize(fs: Seq[Logarithm[F1]]) = Logarithm(canVectorizeF1.vectorize(fs.map(_.fn)))
    }

  implicit def canVectorizePower[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2], LF1 <: SymbolicFunction[LF1], LF2 <: SymbolicFunction[LF2]](
    implicit canVectorizeF1: CanVectorize.Aux[F1, LF1],
    canVectorizeF2: CanVectorize.Aux[F2, LF2]
  ): CanVectorize.Aux[Power[F1, F2], Power[LF1, LF2]] =
    new AbstractCanVectorize[Power[F1, F2]]("Power[F1, F2]") {
      type V = Power[LF1, LF2]
      def vectorize(fs: Seq[Power[F1, F2]]) = Power(canVectorizeF1.vectorize(fs.map(_.fn)), canVectorizeF2.vectorize(fs.map(_.exp)))
    }

  implicit def canVectorizeProduct[L <: HList, VL <: HList](
    implicit canVectorizeHList: CanVectorize.Aux[L, VL],
    vectorizedListAllSymbolicFunction: LUBConstraint[VL, SymbolicFunction[_]]
  ): CanVectorize.Aux[Product[L], Product[VL]] =
    new AbstractCanVectorize[Product[L]]("Product[L]") {
      type V = Product[VL]
      def vectorize(fs: Seq[Product[L]]): V = Product(canVectorizeHList.vectorize(fs.map(_.fns)))
    }

  implicit def canVectorizeDivision[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2], LF1 <: SymbolicFunction[LF1], LF2 <: SymbolicFunction[LF2]](
    implicit canVectorizeF1: CanVectorize.Aux[F1, LF1],
    canVectorizeF2: CanVectorize.Aux[F2, LF2]
  ): CanVectorize.Aux[Division[F1, F2], Division[LF1, LF2]] =
    new AbstractCanVectorize[Division[F1, F2]]("Division[F1, F2]") {
      type V = Division[LF1, LF2]
      def vectorize(fs: Seq[Division[F1, F2]]) = Division(canVectorizeF1.vectorize(fs.map(_.fn1)), canVectorizeF2.vectorize(fs.map(_.fn2)))
    }


  implicit def canVectorizeSum[L <: HList, VL <: HList](
    implicit canVectorizeHList: CanVectorize.Aux[L, VL],
    vectorizedListAllSymbolicFunction: LUBConstraint[VL, SymbolicFunction[_]]
  ): CanVectorize.Aux[Sum[L], Sum[VL]] =
    new AbstractCanVectorize[Sum[L]]("Sum[L]") {
      type V = Sum[VL]
      def vectorize(fs: Seq[Sum[L]]): V = Sum(canVectorizeHList.vectorize(fs.map(_.fns)))
    }

  implicit def canVectorizeDifference[F1 <: SymbolicFunction[F1], F2 <: SymbolicFunction[F2], LF1 <: SymbolicFunction[LF1], LF2 <: SymbolicFunction[LF2]](
    implicit canVectorizeF1: CanVectorize.Aux[F1, LF1],
    canVectorizeF2: CanVectorize.Aux[F2, LF2]
  ): CanVectorize.Aux[Difference[F1, F2], Difference[LF1, LF2]] =
    new AbstractCanVectorize[Difference[F1, F2]]("Difference[F1, F2]") {
      type V = Difference[LF1, LF2]
      def vectorize(fs: Seq[Difference[F1, F2]]) = Difference(canVectorizeF1.vectorize(fs.map(_.fn1)), canVectorizeF2.vectorize(fs.map(_.fn2)))
    }

  implicit def canVectorizeDoublesToDenseVector: CanVectorize.Aux[Double, DenseVector[Double]] =
    new AbstractCanVectorize[Double]("Double") {
      type V = DenseVector[Double]
      override def vectorize(fs: Seq[Double]): DenseVector[Double] =
        DenseVector(fs: _*)
    }
}

object CanVectorize extends LowPriorityCanVectorize {
  type Aux[E, V0] = CanVectorize[E] { type V = V0 }
  implicit class RichVectorizable[E](fs: Seq[E]) {
    def vectorized(implicit canVectorize: CanVectorize[E]): canVectorize.V = canVectorize.vectorize(fs)
  }
  case class SingletonOf[T, U <: { type V }](widen: T { type V = U#V })
  object SingletonOf {
    implicit def mkSingleton[T <: {type V}](implicit t: T): SingletonOf[T, t.type] =
      SingletonOf[T, t.type](t)
  }


  implicit def canVectorizeLabelledHList[K, V0, L <: HList, TL <: HList, KL <: HList, LL <: HList](
    implicit canVectorizeHead: CanVectorize[V0],
    canVectorizeTail: CanVectorize.Aux[L, TL],
    keys: Keys.Aux[FieldType[K, V0] :: L, KL],
    isHCons: IsHCons[KL]
  ): CanVectorize.Aux[FieldType[K, V0] :: L, FieldType[isHCons.H, canVectorizeHead.V] :: TL] =
    new AbstractCanVectorize[FieldType[K, V0] :: L]("FieldType[K, V0] :: L") {
      type V = FieldType[isHCons.H, canVectorizeHead.V] :: TL
      def vectorize(fs: Seq[FieldType[K, V0] :: L]): V = {
        val headKey: isHCons.H = keys().head
        val headValue: canVectorizeHead.V = canVectorizeHead.vectorize(fs.map(_.head))
        val tails = fs.map(_.tail)
        (headValue.asInstanceOf[FieldType[isHCons.H, canVectorizeHead.V]]) :: canVectorizeTail.vectorize(tails)
      }
    }

}

object vectorize extends UFunc with MappingUFunc {
  implicit def implCanVectorize[F](implicit canVectorize: CanVectorize[F]): Impl[Seq[F], canVectorize.V] = new Impl[Seq[F], canVectorize.V] {
    def apply(f: Seq[F]) = canVectorize.vectorize(f)
  }
}