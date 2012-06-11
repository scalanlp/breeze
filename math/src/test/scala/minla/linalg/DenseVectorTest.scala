package minla.linalg

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class DenseVectorTest extends FunSuite with Checkers {

  val TOLERANCE = 1e-4

  def assertClose(a: Double, b: Double) =
    assert(math.abs(a - b) < TOLERANCE)

  test("Min/Max") {
    val v = DenseVector(2, 0, 3, 2, -1)
    assert(v.argmin === 4)
    assert(v.argmax === 2)
    assert(v.min === -1)
    assert(v.max === 3)
  }

  test("Norm") {
    val v = DenseVector(-0.4326, -1.6656, 0.1253, 0.2877, -1.1465)
    assertClose(v.norm(1), 3.6577)
    assertClose(v.norm(2), 2.0915)
    assertClose(v.norm(3), 1.8405)
    assertClose(v.norm(4), 1.7541)
    assertClose(v.norm(5), 1.7146)
    assertClose(v.norm(6), 1.6940)
    assertClose(v.norm(Double.PositiveInfinity), 1.6656)
  }

  test("MulInner") {
    val a = DenseVector(0.56390, 0.36231, 0.14601, 0.60294, 0.14535)
    val b = DenseVector(0.15951, 0.83671, 0.56002, 0.57797, 0.54450)
    assertClose(a dot b, .90249)
  }

  test("MulOuter") {
    val a = DenseVector(1., 2., 3.)
    val b = DenseVector(6., -4., 8.)

    // assert result is a dense matrix
    val m: DenseMatrix[Double] = a * b.t
    assert(m === DenseMatrix((6., -4., 8.), (12., -8., 16.), (18., -12., 24.)))
  }

  test("Slice") {
    val x = DenseVector.zeros[Int](5)

    // check that the slice is a vector and mutable
    val y = x(0 to 2)
    y :+= 1

    val z = x(1 to 3)
    z :+= 1

    assert(x === DenseVector(1, 2, 2, 1, 0))

    assert(x(0 until 5) === x)
    assert(try {
      x(0 to 5); false
    } catch {
      case _ => true
    })
  }

  test("Slice and Transpose") {
    val x = DenseVector[Double](1, 2, 3, 4, 5)

    val s: DenseVector[Double] = x(2 to 3)

    assert(s === DenseVector(3., 4.))

    val t = s.t

    assert(t === DenseVector(3., 4.).t)

    val emptySlice = x(2 until 2)
    assert(emptySlice === DenseVector[Double]())
  }

  test("Transpose") {
    val x: DenseVector[Double] = DenseVector(1, 2, 3)

    // test static type and write-through of transpose
    val y = x.t
    y(0, 0) = 0
    assert(x === DenseVector(0., 2., 3.))
  }

  test("MapValues") {
    val a: DenseVector[Double] = DenseVector(1, 2, 3, 4, 5)
    val m: DenseVector[Double] = a.mapValues(_ + 1)
    assert(m === DenseVector(2., 3., 4., 5., 6.))
  }

  test("ForComprehensions") {
    val a: DenseVector[Double] = DenseVector(1, 2, 3, 4, 5)

    var s = 0.0

    // foreach
    s = 0.0
    for (v <- a) s += v
    assert(s === a.sum)

//    filter
//    s = 0.0
//    for (v <- a if v < 3) s += v
//    assert(s === 1 + 2)

    // map
    val b1: DenseVector[Double] = for (v <- a) yield v * 2.0
    assert(b1 === DenseVector(2.0, 4.0, 6.0, 8.0, 10.0))

//    map with filter
//    val b2: DenseVector[Double] = for (v <- a if v < 3) yield v * 2
//    assert(b2 === DenseVector(2.0, 4.0))
  }

  test("Tabulate") {
    val m = DenseVector.tabulate(5)(i => i + 1)
    assert(m === DenseVector(1, 2, 3, 4, 5))
  }

  test("VertCat") {
    val a1 = DenseVector[Double](1, 2, 3)
    val a2 = DenseVector[Double](2, 3, 4)
    val res = DenseVector[Double](1, 2, 3, 2, 3, 4)
    assert(DenseVector.vertcat(a1, a2) === res)
  }

  test("Negation") {
    val a1 = DenseVector(1.0, 2.0, 3.0)
    assert(-a1 == DenseVector(-1.0, -2.0, -3.0))

  }
}
