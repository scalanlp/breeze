package breeze.linalg

import org.scalacheck._
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._
import org.junit.runner.RunWith
import breeze.math.{Complex, DoubleValuedTensorSpaceTestBase, TensorSpace, TensorSpaceTestBase}
import java.util

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class DenseVectorTest extends FunSuite with Checkers {

  val TOLERANCE = 1e-4

  def assertClose(a: Double, b: Double) =
    assert(math.abs(a - b) < TOLERANCE)
    
  def assertClose(a: Complex, b: Complex) =
    assert(math.abs(a.real - b.real) < TOLERANCE && math.abs(a.imag - b.imag) < TOLERANCE)

  test("Can raise IntegerVector by Integer") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val w = v :^ 2
    assert(w == DenseVector(4, 0, 9, 4, 1))
  }

  test("Can raise DoubleVector by Double") {
    val v = DenseVector(2d, 0d, 3d, 2d, -1d)
    val w = v :^ 2d
    assert(w == DenseVector(4d, 0d, 9d, 4d, 1d))
  }

  test("Can raise FloatVector by Float") {
    val v = DenseVector(2f, 0f, 3f, 2f, -1f)
    val w = v :^ 2f
    assert(w == DenseVector(4f, 0f, 9f, 4f, 1f))
  }
  
  test("Can raise ComplexVector by Complex") {
    val v = DenseVector(Complex(0,0), Complex(1,1), Complex(2,2), Complex(-1,-1))
    val w = v :^ Complex(2,0)
    assertClose(w(0), Complex(0,0))
    assertClose(w(1), Complex(0,2))
    assertClose(w(2), Complex(0,8))
    assertClose(w(3), Complex(0,2))
  }

  test("Min/Max") {
    val v = DenseVector(2, 0, 3, 2, -1)
    assert(v.argmin === 4)
    assert(v.argmax === 2)
    assert(v.min === -1)
    assert(v.max === 3)
  }

  test("Topk") {
    val v = DenseVector(2, 0, 3, 4, -1)

    assert(v.argtopk(3) === IndexedSeq(3,2,0))
  }


  test("Mean") {
    assert(mean(DenseVector(0.0,1.0,2.0)) === 1.0)
    assert(mean(DenseVector(0.0,3.0)) === 1.5)
    assert(mean(DenseVector(3.0)) === 3.0)
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
    val a = DenseVector(1.0, 2.0, 3.0)
    val b = DenseVector(6.0, -4.0, 8.0)

    // assert result is a dense matrix
    val m: DenseMatrix[Double] = a * b.t
    assert(m === DenseMatrix((6.0, -4.0, 8.0), (12.0, -8.0, 16.0), (18.0, -12.0, 24.0)))
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
      case _: Throwable => true
    })
  }

  test("Slice and Transpose") {
    val x = DenseVector[Double](1, 2, 3, 4, 5)

    val s: DenseVector[Double] = x(2 to 3)
    val s2: DenseVector[Double] = x.slice(2,4)

    assert(s === DenseVector(3.0, 4.0))
    assert(s2 === DenseVector(3.0, 4.0))

    val t = s.t

    assert(t === DenseVector(3.0, 4.0).t)

    val emptySlice = x(2 until 2)
    val emptySlice2 = x.slice(2,2)
    assert(emptySlice === DenseVector[Double]())
    assert(emptySlice2 === DenseVector[Double]())
  }

  test("Slice and Transpose Int") {
    val x = DenseVector[Int](1, 2, 3, 4, 5)

    val s: DenseVector[Int] = x(2 to 3)

    assert(s === DenseVector(3, 4))

    val t = s.t

    assert(t === DenseVector(3, 4).t)

    val emptySlice = x(2 until 2)
    assert(emptySlice === DenseVector[Int]())
  }


  test("Slice and Transpose Float") {
    val x = DenseVector[Float](1, 2, 3, 4, 5)

    val s: DenseVector[Float] = x(2 to 3)

    assert(s === DenseVector(3f, 4f))

    val t = s.t

    assert(t === DenseVector(3f, 4f).t)

    val emptySlice = x(2 until 2)
    assert(emptySlice === DenseVector[Float]())
  }

  test("Transpose") {
    val x: DenseVector[Double] = DenseVector(1, 2, 3)

    // test static type and write-through of transpose
    val y = x.t
    y(0, 0) = 0
    assert(x === DenseVector(0.0, 2.0, 3.0))
  }
  
  test("Transpose Complex") {
    val x = DenseVector(Complex(1, 1), Complex(1, -1))
    val y = x.t
    val expected = new DenseMatrix(1, 2, Array(Complex(1, -1), Complex(1, 1)))
    assert(y === expected)
  }

  test("Map(Active)Pairs Double") {
    val a: DenseVector[Double] = DenseVector(1, 2, 3, 4, 5)
    val mv: DenseVector[Double] = a.mapPairs((i,x) => x + 1)
    val mav: DenseVector[Double] = a.mapActivePairs((i,x) => x + 1)
    assert(mv === DenseVector(2.0, 3.0, 4.0, 5.0, 6.0))
    assert(mav === DenseVector(2.0, 3.0, 4.0, 5.0, 6.0))
  }

  test("Map(Active)Values Double") {
    val a: DenseVector[Double] = DenseVector(1, 2, 3, 4, 5)
    val mv: DenseVector[Double] = a.mapValues(_ + 1)
    val mav: DenseVector[Double] = a.mapActiveValues(_ + 1)
    assert(mv === DenseVector(2.0, 3.0, 4.0, 5.0, 6.0))
    assert(mav === DenseVector(2.0, 3.0, 4.0, 5.0, 6.0))
  }

  test("Map(Active)Pairs Int") {
    val a: DenseVector[Int] = DenseVector(1, 2, 3, 4, 5)
    val mv: DenseVector[Int] = a.mapPairs((i,x) => x + 1)
    val mav: DenseVector[Int] = a.mapActivePairs((i,x) => x + 1)
    assert(mv === DenseVector(2, 3, 4, 5, 6))
    assert(mav === DenseVector(2, 3, 4, 5, 6))
  }

  test("Map(Active)Values Int") {
    val a: DenseVector[Int] = DenseVector(1, 2, 3, 4, 5)
    val mv: DenseVector[Int] = a.mapValues(_ + 1)
    val mav: DenseVector[Int] = a.mapActiveValues(_ + 1)
    assert(mv === DenseVector(2, 3, 4, 5, 6))
    assert(mav === DenseVector(2, 3, 4, 5, 6))
  }

  test("Map(Active)Pairs Float") {
    val a: DenseVector[Float] = DenseVector(1, 2, 3, 4, 5)
    val mv: DenseVector[Float] = a.mapPairs((i,x) => x + 1)
    val mav: DenseVector[Float] = a.mapActivePairs((i,x) => x + 1)
    assert(mv === DenseVector(2f, 3f, 4f, 5f, 6f))
    assert(mav === DenseVector(2f, 3f, 4f, 5f, 6f))
  }

  test("Map(Active)Values Float") {
    val a: DenseVector[Float] = DenseVector(1f, 2f, 3f, 4f, 5f)
    val mv: DenseVector[Float] = a.mapValues(_ + 1f)
    val mav: DenseVector[Float] = a.mapActiveValues(_ + 1f)
    assert(mv === DenseVector(2f, 3f, 4f, 5f, 6f))
    assert(mav === DenseVector(2f, 3f, 4f, 5f, 6f))
  }
  
  test("Map(Active)Values Complex") {
    val a: DenseVector[Complex] = DenseVector(Complex(1,1), Complex(2,2))
    val mv: DenseVector[Complex] = a.mapValues(_ + Complex(1,1))
    val mav: DenseVector[Complex] = a.mapActiveValues(_ + Complex(1,1))
    assert(mv === DenseVector(Complex(2,2), Complex(3,3)))
    assert(mav === DenseVector(Complex(2,2), Complex(3,3)))
  }

  test("ForComprehensions") {
    val a: DenseVector[Int] = DenseVector(1, 2, 3, 4, 5)

    var s = 0.0

    // foreach
    s = 0.0
    for (v <- a) s += v
    assert(s === sum(a))

//    filter
//    s = 0.0
//    for (v <- a if v < 3) s += v
//    assert(s === 1 + 2)

    // map
    val b1: DenseVector[Int] = for (v <- a) yield v * 2
    assert(b1 === DenseVector(2, 4, 6, 8, 10))

//    map with filter
//    val b2: DenseVector[Int] = for (v <- a if v < 3) yield v * 2
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


  test("horzcat") {
    val a1 = DenseVector[Double](1, 2, 3)
    val a2 = DenseVector[Double](2, 3, 4)
    val res = DenseMatrix( (1.0, 2.0), (2.0, 3.0), (3.0, 4.0))
    assert(DenseVector.horzcat(a1, a2) === res)
  }

  test("Negation") {
    val a1 = DenseVector(1.0, 2.0, 3.0)
    assert(-a1 == DenseVector(-1.0, -2.0, -3.0))

  }

  test("DV ops work as Vector") {
    val a = DenseVector(1.0, 2.0, 3.0)
    val b = DenseVector(3.0, 4.0, 5.0)
    (a:Vector[Double]) += (b: Vector[Double])
    assert(a === DenseVector(4.0,6.0,8.0))
    assert((a: Vector[Double]).dot (b: Vector[Double]) === (a dot b))
    (a:Vector[Double]) *= (b: Vector[Double])
    assert(a === DenseVector(12.0,24.0,40.0))
  }

  test("Generic DV ops") {
    // mostly for coverage
    val a = DenseVector("SSS")
    assert(a.copy === a)
    intercept[IndexOutOfBoundsException] {
      a(3) = ":("
      assert(false, "Shouldn't be here!")
    }
    assert(a(0) === "SSS")
    intercept[IndexOutOfBoundsException] {
      a(3)
      assert(false, "Shouldn't be here!")
    }


  }
  
  test("Complex OpSet") {
    val a = DenseVector(Complex(1,1))
    val b = DenseVector(Complex(2,2))
    a := b
    assert(a === b)
  }

  test("Equals")  {
    val a, b = DenseVector(3,4,5,6)
    assert(a === b)
    a(0) = -1
    assert(a.slice(1, a.length) === b.slice(1, b.length))
    a.slice(0,3) := b.slice(1,b.length)
    assert(a.slice(0, 3) === b.slice(1, b.length))
    a.slice(0,4,2) := b(0 until 2)
    assert(a.slice(0, 4, 2) === b(0 until 2))
  }

  test("toArray") {
    val a = DenseVector(1, 2, 3)
    assert(util.Arrays.equals(a.toArray, a.data))
    assert(util.Arrays.equals(a(0 until 3 by 2).toArray, Array(1,3)))
    assert(util.Arrays.equals(a(1 until 3 by 1).toArray, Array(2,3)))
  }

  test("OpEq and friends") {
    val a = DenseVector(1, 2, 3)
    val b = DenseVector(1, 4, 1)
    assert( (a :== b) === BitVector(true, false, false))
    assert( (a :!= b) === BitVector(false, true, true))
    assert( (a :<= b) === BitVector(true, true, false))
    assert( (a :>= b) === BitVector(true, false, true))
    assert( (a :< b) === BitVector(false, true, false))
    assert( (a :> b) === BitVector(false, false, true))
  }

  test("clip") {
    val dv = DenseVector.range(0, 10)
    assert(clip(dv, 1, 8) === DenseVector(1,1,2,3,4,5,6,7,8,8))
    clip.inPlace(dv, 1, 8)
    assert(dv === DenseVector(1,1,2,3,4,5,6,7,8,8))
  }

  test("any and all") {
    val a = DenseVector(1, 2, 3)
    val b = DenseVector(1, 4, 1)
    assert( any(a :== b))
    assert( !all(a :== b))
    assert( any(a :== a))
    assert( all(a :== a))
    assert( !any(a :== (b - 1)))
  }

}

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class DenseVectorOps_DoubleTest extends DoubleValuedTensorSpaceTestBase[DenseVector[Double], Int] {
 val space: TensorSpace[DenseVector[Double], Int, Double] = implicitly

  val N = 30
  implicit def genTriple: Arbitrary[(DenseVector[Double], DenseVector[Double], DenseVector[Double])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Double].map { _  % 1E100}
          y <- Arbitrary.arbitrary[Double].map { _ % 1E100 }
          z <- Arbitrary.arbitrary[Double].map { _ % 1E100 }
      } yield {
        (DenseVector.fill(N)(math.random * x),
          DenseVector.fill(N)(math.random * y),
          DenseVector.fill(N)(math.random * z))
      }
    }
  }

  def genScalar: Arbitrary[Double] = Arbitrary(Arbitrary.arbitrary[Double].map{ _ % 1E10 })
}

@RunWith(classOf[JUnitRunner])
class DenseVectorOps_IntTest extends TensorSpaceTestBase[DenseVector[Int], Int, Int] {
 val space: TensorSpace[DenseVector[Int], Int, Int] = implicitly

  val N = 30
  implicit def genTriple: Arbitrary[(DenseVector[Int], DenseVector[Int], DenseVector[Int])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Int].map { _  % 1000}
          y <- Arbitrary.arbitrary[Int].map { _ % 1000}
          z <- Arbitrary.arbitrary[Int].map { _ % 1000}
      } yield {
        (DenseVector.fill(N)(math.random * x toInt),
          DenseVector.fill(N)(math.random * y toInt),
          DenseVector.fill(N)(math.random * z toInt))
      }
    }
  }

  def genScalar: Arbitrary[Int] = Arbitrary(Arbitrary.arbitrary[Int].map{ _ % 1000 })
}

@RunWith(classOf[JUnitRunner])
class DenseVectorOps_FloatTest extends TensorSpaceTestBase[DenseVector[Float], Int, Float] {
 val space: TensorSpace[DenseVector[Float], Int, Float] = implicitly

  val N = 30
  implicit def genTriple: Arbitrary[(DenseVector[Float], DenseVector[Float], DenseVector[Float])] = {
    Arbitrary {
      for{x <- Arbitrary.arbitrary[Float].map { _  % 1000}
          y <- Arbitrary.arbitrary[Float].map { _ % 1000}
          z <- Arbitrary.arbitrary[Float].map { _ % 1000}
      } yield {
        (DenseVector.fill(N)(math.random * x toFloat),
          DenseVector.fill(N)(math.random * y toFloat),
          DenseVector.fill(N)(math.random * z toFloat))
      }
    }
  }

  def genScalar: Arbitrary[Float] = Arbitrary(Arbitrary.arbitrary[Float].map{ _ % 1000 })
}
