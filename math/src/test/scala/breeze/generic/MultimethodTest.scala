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
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class MultimethodTest extends FunSuite with Checkers {
  trait T
  trait V extends T
  trait DV extends V
  trait SV extends V
  trait M extends T
  trait DM extends M

  class MM extends Multimethod2[Function2, T, T, Symbol] with ((T,T)=>Symbol)
  class MMPrim extends Multimethod2[Function2, T, Double, Symbol] with ((T,Double)=>Symbol)
  class MMDouble extends Multimethod2[Function2, Double, Double, Symbol] with ((Double,Double)=>Symbol)

  val t = new T{}
  val v = new V{}
  val dv = new DV{}
  val sv = new SV{}
  val m = new M{}
  val dm = new DM{}

  val all = Set(t, v, dv, sv, m, dm)

  test("exceptions for things not present") {
    val mm = new MM
    for(a <- all; b <- all) {
      intercept[UnsupportedOperationException] {
        mm(a, b)
        assert(false, "Shouldn't be here!")
      }
    }

    mm.register({(a: DV, b: DV) => 'Woo})
    for(a <- all; b <- all if a != dv && b != dv) {
      intercept[UnsupportedOperationException] {
        mm(a, b)
        assert(false, "Shouldn't be here!")
      }
    }
  }

  test("basics") {
    val mm = new MM

    mm.register({(a: DV, b: DV) => 'Woo})
    assert(mm(dv, dv) === 'Woo)

    mm.register({(a: DV, b: SV) => 'Yay})
    assert(mm(dv, sv) === 'Yay)

    mm.register({(a: DV, b: M) => 'Ok})
    assert(mm(dv, m) === 'Ok)
  }

  test("inheritance") {
    val mm = new MM

    mm.register({(a: V, b: V) => 'Woo})
    assert(mm(dv, dv) === 'Woo)

    mm.register({(a: DV, b: SV) => 'Yay})
    assert(mm(dv, sv) === 'Yay)
    assert(mm(dv, dv) === 'Woo)

    mm.register({(a: V, b: M) => 'Ok})
    assert(mm(dv, m) === 'Ok)
  }


  test("primitives on second type") {
    val mm = new MMPrim

    mm.register({(a: V, b: Double) => 'Woo})
    assert(mm(dv, 4.0) === 'Woo)

    mm.register({(a: DV, b: Double) => 'Yay})
    assert(mm(dv, 4.0) === 'Yay)
    assert(mm(sv, 3.0) === 'Woo)
  }

  test("double primitives") {
    val mm = new MMDouble()
    mm.register((a: Double, b: Double) => 'Woo)
    assert(mm(5.0, 4.0) === 'Woo)
  }
}
