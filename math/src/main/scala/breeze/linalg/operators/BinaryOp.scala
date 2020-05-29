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

import breeze.generic.UFunc.UImpl2
import breeze.generic.{UFunc, MMRegistry2 /*, Multimethod2*/}
import breeze.linalg.support.CanCopy

import scala.annotation.unchecked.uncheckedVariance
import scala.reflect.ClassTag

object BinaryOp {

  def fromCopyAndUpdate[A, B, Op](implicit
      op: UFunc.InPlaceImpl2[Op, A, B],
      copy: CanCopy[A]
  ): UFunc.UImpl2[Op, A, B, A] = {
    new UFunc.UImpl2[Op, A, B, A] {
      def apply(a: A, b: B): A = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }

}

/**
 * This is a special kind of BinaryOp that supports registration
 * of specialized implementations for a given operation.
 * @author dlwh
 */
// This trait could reuse code from Multimethod2, but not doing so allows us to reduce code size a lot
// because we don't need BinaryOp's to inherit from Function2, which has a lot of @specialzied cruft.
trait BinaryRegistry[A, B, Op, +R]
    extends UFunc.UImpl2[Op, A, B, R]
    with MMRegistry2[UFunc.UImpl2[Op, _ <: A, _ <: B, _ <: (R @uncheckedVariance)]] {
  protected def bindingMissing(a: A, b: B): R =
    throw new UnsupportedOperationException("Types not found!" + a + b + " " + ops)

  protected def multipleOptions(
      a: A,
      b: B,
      m: Map[(Class[_], Class[_]), UImpl2[Op, _ <: A, _ <: B, _ <: R @uncheckedVariance]]
  ) = {
    throw new RuntimeException("Multiple bindings for method: " + m)
  }

  private val l1cache
      : ThreadLocal[((Class[_], Class[_]), Option[UImpl2[Op, _ <: A, _ <: B, _ <: R @uncheckedVariance]])] = {
    new ThreadLocal[((Class[_], Class[_]), Option[UImpl2[Op, _ <: A, _ <: B, _ <: R]])]
  }

  def apply(a: A, b: B): R = {
    val ac = a.asInstanceOf[AnyRef].getClass
    val bc = b.asInstanceOf[AnyRef].getClass
    val pair = (ac, bc)

    val firstLevelCached = l1cache.get()
    if (firstLevelCached != null && pair == firstLevelCached._1) {
      firstLevelCached._2 match {
        case None => bindingMissing(a, b)
        case some @ Some(m) =>
          m.asInstanceOf[UImpl2[Op, A, B, R]].apply(a, b)
      }
    } else {
      slowPath(a, b, ac, bc, pair)
    }

  }

  private def slowPath(
      a: A,
      b: B,
      ac: Class[_ <: AnyRef],
      bc: Class[_ <: AnyRef],
      pair: (Class[_ <: AnyRef], Class[_ <: AnyRef])
  ): R = {
    val cached: Option[UImpl2[Op, _ <: A, _ <: B, _ <: R @uncheckedVariance]] = cache.get(pair)
    if (cached != null) {
      cached match {
        case None => bindingMissing(a, b)
        case some @ Some(m) =>
          l1cache.set(pair -> some)
          m.asInstanceOf[UImpl2[Op, A, B, R]].apply(a, b)
      }
    } else {
      val options = resolve(ac, bc.asInstanceOf[Class[_ <: B]])

      options.size match {
        case 0 =>
          cache.put(ac -> bc, None)
          bindingMissing(a, b)
        case 1 =>
          val method = options.values.head
          cache.put(ac -> bc, Some(method))
          method.asInstanceOf[UImpl2[Op, A, B, R]].apply(a, b)
        case _ =>
          val selected = selectBestOption(options)
          if (selected.size != 1)
            multipleOptions(a, b, options)
          else {
            val method = selected.values.head
            val some = Some(method)
            l1cache.set(pair -> some)
            cache.put(pair, some)
            method.asInstanceOf[UImpl2[Op, A, B, R]].apply(a, b)
          }
      }
    }
  }

  def register[AA <: A, BB <: B](
      op: UImpl2[Op, AA, BB, _ <: R @uncheckedVariance]
  )(implicit cA: ClassTag[AA], cB: ClassTag[BB]) = {
    super.register(cA.runtimeClass, cB.runtimeClass, op)
    op
  }

}
