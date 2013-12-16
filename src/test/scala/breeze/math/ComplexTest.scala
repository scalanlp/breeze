/*
 * Distributed as part of Scalala, a linear algebra library.
 *
 * Copyright (C) 2008- Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package breeze.math



import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import breeze.linalg.DenseVector

@RunWith(classOf[JUnitRunner])
class ComplexTest extends FunSuite with Checkers {

  test("Add") {
    assert((1 + 2 * i) + (2 + 3 * i) === (3 + 5 * i))
  }

  test("Sub") {
    assert((1 + 2 * i) - (2 + 3 * i) === (-1 - i))
  }

  test("Div") {
    assert((5 + 10 * i) / (3 - 4 * i) === (-1 + 2 * i))
  }

  test("Mul") {
    assert((1 + 2 * i) * (-3 + 6 * i) === -15)
    assert((1 + 5 * i) * (-3 + 2 * i) === (-13 - 13 * i))
  }

  test("Neg") {
    assert(-(1 + 2 * i) === (-1 - 2 * i))
  }

  test("Abs/Conj") {
    assert((3 + 4 * i).abs === 5)
    val c = (1.7 + 2.1 * i)
    assert(c * c.conjugate === 7.3)
  }

  test("List[Complex].sum (test ComplexIsFractional)") {
    val x = List((5 + 7 * i), (1 + 3 * i), (13 + 17 * i))
    assert(x.sum === (19 + 27 * i))
  }

  test("List[Complex].product (test ComplexIsFractional)") {
    val x = List((5 + 7 * i), (1 + 3 * i), (13 + 17 * i))
    assert(x.product === (-582 + 14 * i))
  }

  test("List[Complex].sorted (test ComplexOrdering)") {
    val x = List((5 + 7 * i), (1 + 3 * i), (13 + 17 * i))
    assert(x.sorted === List((1 + 3 * i), (5 + 7 * i), (13 + 17 * i)))
  }

  test("universal function compatibility") {
    import breeze.numerics.abs
    assert(abs(DenseVector(Complex.i)) === DenseVector(1.0))
  }

  test("Only permit conversion to built-in numeric types when purely real.") {
    val a = 5 + 0 * i // should allow conversion
    val b = 5 + 5 * i // should bork with an IllegalArgumentException

    def toDouble[Complex](c: Complex)(implicit n: Numeric[Complex]): Double =
      n.toDouble(c)
    def toFloat[Complex](c: Complex)(implicit n: Numeric[Complex]): Float =
      n.toFloat(c)
    def toInt[Complex](c: Complex)(implicit n: Numeric[Complex]): Int =
      n.toInt(c)
    def toLong[Complex](c: Complex)(implicit n: Numeric[Complex]): Long =
      n.toLong(c)

    assert(toDouble(a) === 5)
    assert(toFloat(a) === 5)
    assert(toInt(a) === 5)
    assert(toLong(a) === 5)
    intercept[IllegalArgumentException] {
      toDouble(b)
    }
    intercept[IllegalArgumentException] {
      toFloat(b)
    }
    intercept[IllegalArgumentException] {
      toInt(b)
    }
    intercept[IllegalArgumentException] {
      toLong(b)
    }
  }

}
