package breeze.collection.mutable

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
import org.scalatestplus.scalacheck.Checkers
import org.scalatest.funsuite.AnyFunSuite
import org.scalacheck.{Arbitrary, Prop}

/**
 *
 * @author dlwh
 */
class OpenAddressHashArrayTest extends AnyFunSuite with Checkers {
  test("add stuff, make sure it and only it is there") {
    implicit val arbInt: Arbitrary[Int] = Arbitrary { Arbitrary.arbInt.arbitrary.map(_.abs % 1000) }
    check {
      Prop.forAll((data: List[Int], size: Int) =>
        size <= 0 || {
          val mdata = data.map(i => math.abs(i) % size).filter(_ >= 0).toSet
          val arr = new OpenAddressHashArray[Int](size)
          assert(arr.size > 0, size)
          for (i <- mdata) {
            arr(i) = i
          }
          mdata.forall(i => arr(i) == i) && !(0 until size).filterNot(mdata).exists(arr(_) != 0)
      })
    }
  }

  test("equals, copy, hashcode") {
    implicit val arbInt: Arbitrary[Int] = Arbitrary { Arbitrary.arbInt.arbitrary.map(_.abs % 1000) }
    check {
      Prop.forAll((data: List[Int], size: Int) =>
        size <= 0 || {
          val mdata = data.map(i => math.abs(i) % size).filter(_ >= 0).toSet
          val arr = new OpenAddressHashArray[Int](size)
          val arr2 = new OpenAddressHashArray[Int](size)
          assert(arr.size > 0, size)
          for (i <- mdata) {
            arr(i) = i
            if (i != 0)
              arr2(i) = i
          }
          (arr == arr.copy && arr.hashCode == arr.copy.hashCode
          && arr2 == arr && arr2.copy.hashCode == arr.copy.hashCode)
      })
    }
  }

  test("configurable default") {
    val oah = new OpenAddressHashArray[Double](10, -1.0, 3)
    assert(oah(0) == -1.0)
    assert(oah(1) == -1.0)
    oah(0) = 3
    assert(oah(0) == 3.0)
    assert(oah(1) == -1.0)
  }

  test("adding default value doesn't do anything") {
    val oah = new OpenAddressHashArray[Double](10, -1.0, 3)
    assert(oah(0) == -1.0)
    assert(oah(1) == -1.0)
    assert(oah.activeSize == 0)

    oah(0) = -1.0
    assert(oah(0) == -1.0)
    assert(oah.activeSize == 0)

    oah(1) = 1.0
    oah(1) = -1.0
    assert(oah(1) == -1.0)
  }
}
