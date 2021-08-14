package breeze.generic
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
import breeze.linalg.operators.BinaryRegistry
import org.scalatest._
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck._

class MultimethodTest extends AnyFunSuite with Checkers {
  trait T
  trait V extends T
  trait DV extends V
  trait SV extends V
  trait M extends T
  trait DM extends M

  trait Tag
  class MM extends BinaryRegistry[T, T, Tag, String]
  class MMPrim extends BinaryRegistry[T, Double, Tag, String]
  class MMDouble extends BinaryRegistry[Double, Double, Tag, String]

  val t = new T {}
  val v = new V {}
  val dv = new DV {}
  val sv = new SV {}
  val m = new M {}
  val dm = new DM {}

  val all = Set(t, v, dv, sv, m, dm)

  private def ufunc[T1, T2, R](f: (T1, T2)=>R):UFunc.UImpl2[Tag, T1, T2, R] = (t1, t2) => f(t1, t2)

  test("exceptions for things not present") {
    val mm = new MM
    for (a <- all; b <- all) {
      intercept[UnsupportedOperationException] {
        mm(a, b)
        assert(false, "Shouldn't be here!")
      }
    }

    mm.register(ufunc { (a: DV, b: DV) =>
      "Woo"
    })
    for (a <- all; b <- all if a != dv && b != dv) {
      intercept[UnsupportedOperationException] {
        mm(a, b)
        assert(false, "Shouldn't be here!")
      }
    }
  }

  test("basics") {
    val mm = new MM

    mm.register(ufunc { (a: DV, b: DV) =>
      "Woo"
    })
    assert(mm(dv, dv) === "Woo")

    mm.register(ufunc { (a: DV, b: SV) =>
      "Yay"
    })
    assert(mm(dv, sv) === "Yay")

    mm.register(ufunc { (a: DV, b: M) =>
      "Ok"
    })
    assert(mm(dv, m) === "Ok")
  }

  test("inheritance") {
    val mm = new MM

    mm.register(ufunc { (a: V, b: V) =>
      "Woo"
    })
    assert(mm(dv, dv) === "Woo")

    mm.register(ufunc { (a: DV, b: SV) =>
      "Yay"
    })
    assert(mm(dv, sv) === "Yay")
    assert(mm(dv, dv) === "Woo")

    mm.register(ufunc{ (a: V, b: M) =>
      "Ok"
    })
    assert(mm(dv, m) === "Ok")
  }

  test("primitives on second type") {
    val mm = new MMPrim

    mm.register(ufunc { (a: V, b: Double) =>
      "Woo"
    })
    assert(mm(dv, 4.0) === "Woo")

    mm.register(ufunc { (a: DV, b: Double) =>
      "Yay"
    })
    assert(mm(dv, 4.0) === "Yay")
    assert(mm(sv, 3.0) === "Woo")
  }

  test("double primitives") {
    val mm = new MMDouble()
    mm.register(ufunc {(a: Double, b: Double) => "Woo"})
    assert(mm(5.0, 4.0) === "Woo")
  }
}
