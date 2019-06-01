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

import breeze.generic.{MMRegistry3, UFunc, MMRegistry2}
import breeze.generic.UFunc.InPlaceImpl3

import scala.reflect.ClassTag

/**
 * This is a special kind of BinaryUpdateOp that supports registration
 * of specialized implementations for a given operation.
 * @author dlwh
 */
// This trait could reuse code from Multimethod2, but not doing so allows us to reduce code size a lot
// because we don't need BinaryOp's to inherit from Function2, which has a lot of @specialzied cruft.
trait TernaryUpdateRegistry[A, B, C, Op]
    extends UFunc.InPlaceImpl3[Op, A, B, C]
    with MMRegistry3[UFunc.InPlaceImpl3[Op, _ <: A, _ <: B, _ <: C]] {
  protected def bindingMissing(a: A, b: B, c: C): Unit =
    throw new UnsupportedOperationException("Types not found!" + a + b + " " + ops)
  protected def multipleOptions(
      a: A,
      b: B,
      c: C,
      m: Map[(Class[_], Class[_], Class[_]), UFunc.InPlaceImpl3[Op, _ <: A, _ <: B, _ <: C]]): Unit = {
    throw new RuntimeException("Multiple bindings for method: " + m)
  }

  def apply(a: A, b: B, c: C): Unit = {
    val ac = a.asInstanceOf[AnyRef].getClass
    val bc = b.asInstanceOf[AnyRef].getClass
    val cc = c.asInstanceOf[AnyRef].getClass

    val cached = cache.get((ac, bc, cc))
    if (cached != null) {
      cached match {
        case None => bindingMissing(a, b, c)
        case Some(m) =>
          m.asInstanceOf[InPlaceImpl3[Op, A, B, C]].apply(a, b, c)
      }
    } else {
      val options = resolve(ac, bc, cc)
      options.size match {
        case 0 =>
          cache.put((ac, bc, cc), None)
          bindingMissing(a, b, c)
        case 1 =>
          val method = options.values.head
          cache.put((ac, bc, cc), Some(method))
          method.asInstanceOf[InPlaceImpl3[Op, A, B, C]].apply(a, b, c)
        case _ =>
          val selected = selectBestOption(options)
          if (selected.size != 1) {
            multipleOptions(a, b, c, options)
          } else {
            val method = selected.values.head
            cache.put((ac, bc, cc), Some(method))
            method.asInstanceOf[InPlaceImpl3[Op, A, B, C]].apply(a, b, c)
          }
      }
    }
  }

  def register[AA <: A, BB <: B, CC <: C](
      op: InPlaceImpl3[Op, AA, BB, CC])(implicit manA: ClassTag[AA], manB: ClassTag[BB], manC: ClassTag[CC]): Unit = {
    super.register(manA.runtimeClass, manB.runtimeClass, manC.runtimeClass, op)
  }
}
