package breeze.linalg.operators
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import breeze.generic.{UFunc, MMRegistry2, Multimethod2}
import breeze.math.{Field, Ring, Semiring}
import breeze.linalg.support.CanCopy
import breeze.numerics.IntMath
import scala.annotation.unchecked.uncheckedVariance

object BinaryOp {
  def fromCopyAndUpdate[A,B,Op<:OpType](implicit op: UFunc.InPlaceImpl2[Op, A, B], copy: CanCopy[A]):UFunc.UImpl2[Op, A, B, A] = {
    new UFunc.UImpl2[Op, A, B, A] {
      def apply(a: A, b: B): A = {
        val c = copy(a)
        op(c,b)
        c
      }
    }


  }

  // scalar implicits
  implicit def scalarOpAdd[S:Semiring]:OpAdd.Impl2[S, S, S] = new OpAdd.Impl2[S, S, S] {
    def apply(a: S, b: S): S = implicitly[Semiring[S]].+(a,b)
  }

  implicit def scalarOpSub[S:Ring]:OpSub.Impl2[S, S, S] = new OpSub.Impl2[S, S, S] {
    def apply(a: S, b: S): S = implicitly[Ring[S]].-(a,b)
  }

  implicit def scalarOpMul[S:Semiring]:OpMulScalar.Impl2[S, S, S] = new OpMulScalar.Impl2[S, S, S] {
    def apply(a: S, b: S): S = implicitly[Semiring[S]].*(a,b)
  }

 implicit def scalarOpMulInner[S:Semiring]:OpMulInner.Impl2[S, S, S] = new OpMulInner.Impl2[S, S, S] {
    def apply(a: S, b: S): S = implicitly[Semiring[S]].*(a,b)
  }

  implicit def scalarOpDiv[S:Field]:OpDiv.Impl2[S, S, S] = new OpDiv.Impl2[S, S, S] {
    def apply(a: S, b: S): S = implicitly[Field[S]]./(a,b)
  }

  implicit def intOpPow:OpPow.Impl2[Int, Int, Int] = new OpPow.Impl2[Int, Int, Int] {
    def apply(a: Int, b: Int): Int = IntMath.ipow(a,b)
  }

  implicit def doubleOpPow:OpPow.Impl2[Double, Double, Double] = new OpPow.Impl2[Double, Double, Double] {
    def apply(a: Double, b: Double): Double = math.pow(a,b)
  }

  implicit def floatOpPow:OpPow.Impl2[Float, Float, Float] = new OpPow.Impl2[Float, Float, Float] {
    def apply(a: Float, b: Float): Float = math.pow(a,b).toFloat
  }

  implicit def intOpMod:OpMod.Impl2[Int, Int, Int] = new OpMod.Impl2[Int, Int, Int] {
    def apply(a: Int, b: Int): Int = IntMath.ipow(a,b)
  }

  implicit def doubleOpMod:OpMod.Impl2[Double, Double, Double] = new OpMod.Impl2[Double, Double, Double] {
    def apply(a: Double, b: Double): Double = math.pow(a,b)
  }

  implicit def floatOpMod:OpMod.Impl2[Float, Float, Float] = new OpMod.Impl2[Float, Float, Float] {
    def apply(a: Float, b: Float): Float = a % b
  }
}

/**
 * This is a special kind of BinaryOp that supports registration
 * of specialized implementations for a given operation.
 * @author dlwh
 */
// This trait could reuse code from Multimethod2, but not doing so allows us to reduce code size a lot
// because we don't need BinaryOp's to inherit from Function2, which has a lot of @specialzied cruft.
trait BinaryRegistry[A, B, Op, +R] extends UFunc.UImpl2[Op, A, B, R] with MMRegistry2[UFunc.UImpl2[Op, _ <: A, _ <: B, _ <: (R @uncheckedVariance)]] {
  protected def bindingMissing(a: A, b: B):R = throw new UnsupportedOperationException("Types not found!" + a + b + " " + ops)
  protected def multipleOptions(a: A, b: B, m: Map[(Class[_],Class[_]),UFunc.UImpl2[Op, _ <: A, _ <: B, _ <: R @uncheckedVariance]]) = {
    throw new RuntimeException("Multiple bindings for method: " + m)
  }

  def apply(a: A, b: B): R = {
    val ac = a.asInstanceOf[AnyRef].getClass
    val bc = b.asInstanceOf[AnyRef].getClass

    val cached = cache.get(ac -> bc)
    if(cached != null) {
      cached match {
        case None => bindingMissing(a, b)
        case Some(m) =>
          m.asInstanceOf[UFunc.UImpl2[Op,A, B, R]].apply(a, b)
      }
    } else {
      val options = resolve(ac, bc.asInstanceOf[Class[_<:B]])

      options.size match {
        case 0 =>
          cache.put(ac -> bc, None)
          bindingMissing(a, b)
        case 1 =>
          val method = options.values.head
          cache.put(ac -> bc, Some(method))
          method.asInstanceOf[UFunc.UImpl2[Op,A, B, R]].apply(a, b)
        case _ =>
          val selected = selectBestOption(options)
          if(selected.size != 1)
            multipleOptions(a, b, options)
          else {
            val method = selected.values.head
            cache.put(ac -> bc, Some(method))
            method.asInstanceOf[UFunc.UImpl2[Op,A, B, R]].apply(a, b)
          }
      }
    }
  }

  def register[AA<:A, BB<:B](op: UFunc.UImpl2[Op,AA, BB, R @uncheckedVariance])(implicit manA: Manifest[AA], manB: Manifest[BB]) = {
    super.register(manA.runtimeClass, manB.runtimeClass, op)
    op
  }

}
