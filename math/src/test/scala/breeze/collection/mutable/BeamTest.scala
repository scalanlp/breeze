package breeze.collection.mutable

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import org.scalacheck._
import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class BeamTest extends FunSuite with Checkers {
  test("creation doesn't go over size") {
    check( Prop.forAll{ (size: Int, cl: List[Int]) => size <= 0 || {
      val beam = Beam[Int](size.abs)(cl:_*)
      beam.size <= size.abs && (cl.size < size.abs || beam.size == size.abs)
    }
    })
  }

  test("addition doesn't go over size") {
    check( Prop.forAll{ (size: Int, cl: List[Int]) => (size <= 0) || {
      val beam = new Beam[Int](size.abs)
      beam ++= cl
      beam.size <= size.abs && (cl.size < size.abs || beam.size == size.abs) && (beam.isEmpty || beam.min == cl.sorted.reverse.take(size).min)
    }
    })
  }


  test("Flatmap") {
    assert(Beam(4)(3.0, 2.0, 1.0).flatMap { i => Iterator(i, i + 1)}  == Beam(4)(4.0, 3.0, 3.0, 2.0))
  }

  test("filter") {
    assert(Beam(4)(3.0, 2.0, 1.0).filter { _ % 2 != 1.0}  == Beam(4)(2.0))
  }
}
