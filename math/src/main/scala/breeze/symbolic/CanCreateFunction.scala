package breeze.symbolic

import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.DenseVector
import breeze.linalg.operators.{OpAdd, OpDiv, OpMulScalar, OpMulMatrix, OpSub}
import breeze.linalg.support.CanCreateZerosLike
import breeze.numerics
import shapeless.LUBConstraint._
import shapeless.{::, HList, HNil}

/**
  * Type-class to create an ordinary Scala function from a SymbolicFunction
  * @tparam I The input type of the Scala function
  * @tparam O The output type of the Scala function
  * @tparam F The type of the SymbolicFunction from which the Scala function is created
  */
trait CanCreateFunction[I, O, F <: SymbolicFunction[F]] {
  def createFunction(f: F): I => O
}

trait LowPriorityCanCreateFunction {
  implicit def canCreateVectorizedSymbolicFunction[I, O, F <: SymbolicFunction[F]](
    implicit canCreateFunction: CanCreateFunction[I, O, F],
    bf: breeze.linalg.support.CanMapKeyValuePairs[DenseVector[I], Int, I, O, DenseVector[O]]
  ): CanCreateFunction[DenseVector[I], DenseVector[O], VectorizedSymbolicFunction[F]] =
    new CanCreateFunction[DenseVector[I], DenseVector[O], VectorizedSymbolicFunction[F]] {
      override def createFunction(f: VectorizedSymbolicFunction[F]): (DenseVector[I]) => DenseVector[O] = {
        val convertedFunctions = f.fs.map(canCreateFunction.createFunction).toArray
        (i: DenseVector[I]) => i.mapPairs { case (index, value) => convertedFunctions(index).apply(value) }
      }
    }

  implicit def canCreateEmptyProductFunction[I, O](
    implicit constOne: CanCreateFunction[I, O, ConstOne]
  ): CanCreateFunction[I, O, Product[HNil]] =
    new CanCreateFunction[I, O, Product[HNil]] {
      def createFunction(f: Product[HNil]) = constOne.createFunction(ConstOne())
    }

  implicit def canCreateProductScalarFunction[I, O1, O2, F <: SymbolicFunction[F], L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit ccf1: CanCreateFunction[I, O1, F],
    ccf2: CanCreateFunction[I, O2, Product[L]],
    multiply: OpMulMatrix.Impl2[O1, O2, O1]
  ) = new CanCreateFunction[I, O1, Product[F :: L]] {
    def createFunction(f: Product[F :: L]) = {
      val f1 = ccf1.createFunction(f.fns.head)
      val f2 = ccf2.createFunction(Product(f.fns.tail))
                                  (i: I) =>
      multiply(f1(i), f2(i))
    }
  }

  implicit def canCreateEmptySumFunction[I, O](
    implicit constZero: CanCreateFunction[I, O, ConstZero]
  ): CanCreateFunction[I, O, Sum[HNil]] =
    new CanCreateFunction[I, O, Sum[HNil]] {
      def createFunction(f: Sum[HNil]) = constZero.createFunction(ConstZero())
    }

  implicit def canCreateSumFunction[I, O, S1 <: SymbolicFunction[S1], S2 <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit ccf1: CanCreateFunction[I, O, S1],
    ccf2: CanCreateFunction[I, O, Sum[S2]],
    add: OpAdd.Impl2[O, O, O]
  ): CanCreateFunction[I, O, Sum[S1 :: S2]] =
    new CanCreateFunction[I, O, Sum[S1 :: S2]] {
      def createFunction(f: Sum[S1 :: S2]) = {
        val f1 = ccf1.createFunction(f.fns.head)
        val f2 = ccf2.createFunction(Sum(f.fns.tail))
        (i: I) =>
          add(f1(i), f2(i))
      }
    }

  implicit def canCreateConstantFunction[I, O]: CanCreateFunction[I, O, Const[O]] =
    new CanCreateFunction[I, O, Const[O]] {
      def createFunction(f: Const[O]) =
        (i: I) => f.const
    }

  implicit def canCreateConstantZeroFunction[I, O](
    implicit zero: CanCreateZerosLike[I, O]
  ): CanCreateFunction[I, O, ConstZero] =
    new CanCreateFunction[I, O, ConstZero] {
      def createFunction(f: ConstZero) =
        (i: I) => zero(i)
    }

  implicit def canCreateConstantOneFunction[I, O](
    implicit zero: CanCreateZerosLike[I, O], exp: numerics.exp.Impl[O, O]
  ): CanCreateFunction[I, O, ConstOne] =
    new CanCreateFunction[I, O, ConstOne] {
      def createFunction(f: ConstOne) =
        (i: I) => exp(zero(i))
    }

  implicit def canCreateVarFunction[T]: CanCreateFunction[T, T, Var] =
    new CanCreateFunction[T, T, Var] {
      def createFunction(f: Var) =
        (i: T) => i
    }

  implicit def canCreateNamedVarFunction[T, S <: Symbol]: CanCreateFunction[T, T, NamedVar[S]] =
    new CanCreateFunction[T, T, NamedVar[S]] {
      def createFunction(f: NamedVar[S]) =
        (i: T) => i
    }

  implicit def canCreateIdentityFunction[T]: CanCreateFunction[T, T, Identity[T]] =
    new CanCreateFunction[T, T, Identity[T]] {
      def createFunction(f: Identity[T]) =
        (i: T) => i
    }

  implicit def canCreateLogarithmFunction[I, O, F <: SymbolicFunction[F]](
    implicit canCreateFunction: CanCreateFunction[I, O, F], log: numerics.log.Impl[O, O]
  ): CanCreateFunction[I, O, Logarithm[F]] =
    new CanCreateFunction[I, O, Logarithm[F]] {
      def createFunction(f: Logarithm[F]) = {
        val fn = canCreateFunction.createFunction(f.fn)
        (i: I) =>
          log(fn(i))
      }
    }

  implicit def canCreateExponentialFunction[I, O, F <: SymbolicFunction[F]](
    implicit canCreateFunction: CanCreateFunction[I, O, F], exp: numerics.exp.Impl[O, O]
  ): CanCreateFunction[I, O, Exponential[F]] =
    new CanCreateFunction[I, O, Exponential[F]] {
      def createFunction(f: Exponential[F]) = {
        val fn = canCreateFunction.createFunction(f.fn)
        (i: I) =>
          exp(fn(i))
      }
    }

  implicit def canCreatePowerFunction[I, O, F <: SymbolicFunction[F], E <: SymbolicFunction[E]](
    implicit canCreateFunction: CanCreateFunction[I, O, F],
    canCreateExpFunction: CanCreateFunction[I, O, E],
    pow: numerics.pow.Impl2[O, O, O]
  ): CanCreateFunction[I, O, Power[F, E]] =
    new CanCreateFunction[I, O, Power[F, E]] {
      def createFunction(f: Power[F, E]) = {
        val fn = canCreateFunction.createFunction(f.fn)
        val exp = canCreateExpFunction.createFunction(f.exp)
        (i: I) =>
          pow(fn(i), exp(i))
      }
    }


  implicit def canCreateDivisionFunction[I, O, N <: SymbolicFunction[N], D <: SymbolicFunction[D]](
    implicit canCreateFunction1: CanCreateFunction[I, O, N],
    canCreateFunction2: CanCreateFunction[I, O, D],
    divide: OpDiv.Impl2[O, O, O]
  ): CanCreateFunction[I, O, Division[N, D]] =
    new CanCreateFunction[I, O, Division[N, D]] {
      def createFunction(f: Division[N, D]) = {
        val f1 = canCreateFunction1.createFunction(f.fn1)
        val f2 = canCreateFunction2.createFunction(f.fn2)
        (i: I) =>
          divide(f1(i), f2(i))
      }
    }

  implicit def canCreateDifferenceFunction[I, O, V <: SymbolicFunction[V], S <: SymbolicFunction[S]](
    implicit canCreateFunction1: CanCreateFunction[I, O, V],
    canCreateFunction2: CanCreateFunction[I, O, S],
    subtract: OpSub.Impl2[O, O, O]
  ): CanCreateFunction[I, O, Difference[V, S]] =
    new CanCreateFunction[I, O, Difference[V, S]] {
      def createFunction(f: Difference[V, S]) = {
        val f1 = canCreateFunction1.createFunction(f.fn1)
        val f2 = canCreateFunction2.createFunction(f.fn2)
        (i: I) =>
          subtract(f1(i), f2(i))
      }
    }

  implicit def canCreateChainFunction[I, O1, O2, Outer <: SymbolicFunction[Outer], Inner <: SymbolicFunction[Inner]](
    implicit canCreateOuterFunction: CanCreateFunction[O1, O2, Outer],
    canCreateInnerFunction: CanCreateFunction[I, O1, Inner]
  ): CanCreateFunction[I, O2, Chain[Outer, Inner]] =
    new CanCreateFunction[I, O2, Chain[Outer, Inner]] {
      def createFunction(f: Chain[Outer, Inner]) = {
        val f1 = canCreateOuterFunction.createFunction(f.outer)
        val f2 = canCreateInnerFunction.createFunction(f.inner)
        f2.andThen(f1)
      }
    }
}

trait MediumPriorityCanCreateFunction extends LowPriorityCanCreateFunction {

  implicit def canCreateProductFunction[I, O, F <: SymbolicFunction[F], L <: HList : <<:[SymbolicFunction[_]]#λ](
    implicit ccf1: CanCreateFunction[I, O, F],
    ccf2: CanCreateFunction[I, O, Product[L]],
    multiply: OpMulScalar.Impl2[O, O, O]
  ): CanCreateFunction[I, O, Product[F :: L]] =
    new CanCreateFunction[I, O, Product[F :: L]] {
      def createFunction(f: Product[F :: L]) = {
        val f1 = ccf1.createFunction(f.fns.head)
        val f2 = ccf2.createFunction(Product(f.fns.tail))
                                    (i: I) =>
        multiply(f1(i), f2(i))
      }
    }

  implicit def canCreateDenseVectorConstantScalarFunction: CanCreateFunction[DenseVector[Double], DenseVector[Double], Const[Double]] =
    new CanCreateFunction[DenseVector[Double], DenseVector[Double], Const[Double]] {
      def createFunction(f: Const[Double]) =
        (i: DenseVector[Double]) => DenseVector.fill(i.length, f.const)
    }

}

object CanCreateFunction extends MediumPriorityCanCreateFunction {
  def apply[I, O, F <: SymbolicFunction[F]](implicit canCreateFunction: CanCreateFunction[I, O, F]) =
    canCreateFunction

  implicit class RichFunctionLike[F <: SymbolicFunction[F]](f: F) {
    def toFunction[I, O](
      implicit canCreateFunction: CanCreateFunction[I, O, F]): I => O =
      canCreateFunction.createFunction(f)
  }

}

object toFunction extends UFunc with MappingUFunc {
  implicit def implCanCreateFunction[I, O, F <: SymbolicFunction[F]](
    implicit canCreateFunction: CanCreateFunction[I, O, F]
  ): Impl[F, I => O] = new Impl[F, I => O] {
    def apply(f: F) = canCreateFunction.createFunction(f)
  }
}
