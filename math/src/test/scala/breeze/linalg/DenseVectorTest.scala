package breeze.linalg

import breeze.numerics.{isNonfinite, sin}
import org.netlib.blas.Ddot
import org.scalacheck._
import org.scalatest._
import org.scalatest.funsuite._
import org.scalatestplus.scalacheck._
import breeze.math._
import breeze.stats.mean

import java.util
import breeze.stats.distributions.RandBasis

import scala.reflect.ClassTag

/**
 *
 * @author dlwh
 */
class DenseVectorTest extends AnyFunSuite with Checkers {

  val TOLERANCE = 1e-4

  def assertClose(a: Double, b: Double) =
    assert(math.abs(a - b) < TOLERANCE)

  def assertClose(a: Complex, b: Complex) =
    assert(math.abs(a.real - b.real) < TOLERANCE && math.abs(a.imag - b.imag) < TOLERANCE)

  test("update/valueAt properly works") {
    val v = DenseVector(2f, 0f, 3f, 2f, -1f)
    v.update(3, 12f)
    assert(v.valueAt(3) == 12f)
  }

  test("update/valueAt properly works with stride, offset") {
    val data = new Array[Double](5 + 3 * 5)
    val v = new DenseVector(data, 5, 3, 5)
    v.update(3, 12)
    assert(v.valueAt(3) == 12)
  }

  test("Can raise IntegerVector by Integer") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val w = v ^:^ 2
    assert(w == DenseVector(4, 0, 9, 4, 1))
  }

  test("Can raise DoubleVector by Double") {
    val v = DenseVector(2d, 0d, 3d, 2d, -1d)
    val w = v ^:^ 2d
    assert(w == DenseVector(4d, 0d, 9d, 4d, 1d))
  }

  test("Can raise FloatVector by Float") {
    val v = DenseVector(2f, 0f, 3f, 2f, -1f)
    val w = v ^:^ 2f
    assert(w == DenseVector(4f, 0f, 9f, 4f, 1f))
  }

  test("Can raise ComplexVector by Complex") {
    val v = DenseVector(Complex(0, 0), Complex(1, 1), Complex(2, 2), Complex(-1, -1))
    val w = v ^:^ Complex(2, 0)
    assertClose(w(0), Complex(0, 0))
    assertClose(w(1), Complex(0, 2))
    assertClose(w(2), Complex(0, 8))
    assertClose(w(3), Complex(0, 2))
  }

  test("Min/Max") {
    val v = DenseVector(2, 0, 3, 2, -1)
    assert(argmin(v) === 4)
    assert(argmax(v) === 2)
    assert(min(v) === -1)
    assert(max(v) === 3)
  }

  test("elemenwise max") {
    val v = DenseVector(2, 0, 3, 2, -1)
    val v2 = DenseVector(3, -1, 3, 4, -4)

    assert(max(v, v2) === DenseVector(3, 0, 3, 4, -1))
    assert(max(v, 2) === DenseVector(2, 2, 3, 2, 2))

    assert(min(v, 2) === DenseVector(2, 0, 2, 2, -1))
  }

  test("Scalars on the LHS") {
    val v = DenseVector(2, 1, 3, 2, -1)
    assert(1 +:+ v == v + 1)
    assert(1 -:- v == -v + 1)
    assert(6 /:/ v == v.mapValues(6 / _))
    assert(6 *:* v == v.mapValues(6 * _))

  }

  test("Topk") {
    val v = DenseVector(2, 0, 3, 4, -1)

    // order doesn't matter
    assert(argtopk(v, 3).toSet === Set(3, 2, 0))
  }

  test("Mean") {
    assert(mean(DenseVector(0.0, 1.0, 2.0)) === 1.0)
    assert(mean(DenseVector(0.0, 3.0)) === 1.5)
    assert(mean(DenseVector(3.0)) === 3.0)
    assert(mean(DenseVector(3.0).t) === 3.0)
  }

  test("Norm") {
    val v = DenseVector(-0.4326, -1.6656, 0.1253, 0.2877, -1.1465)
    assertClose(norm(v, 1), 3.6577)
    assertClose(norm(v, 2), 2.0915)
    assertClose(norm(v, 3), 1.8405)
    assertClose(norm(v, 4), 1.7541)
    assertClose(norm(v, 5), 1.7146)
    assertClose(norm(v, 6), 1.6940)
    assertClose(norm(v.t, 6), 1.6940)
    assertClose(norm(v, Double.PositiveInfinity), 1.6656)
  }

  test("MulInner") {
    val a = DenseVector(0.56390, 0.36231, 0.14601, 0.60294, 0.14535)
    val b = DenseVector(0.15951, 0.83671, 0.56002, 0.57797, 0.54450)
    assertClose(a.t * b, .90249)
    assertClose(a.dot(b), .90249)
  }

  test("MulOuter") {
    val a = DenseVector(1.0, 2.0, 3.0)
    val b = DenseVector(6.0, -4.0, 8.0)

    // assert result is a dense matrix
    val m: DenseMatrix[Double] = a * b.t
    assert(m === DenseMatrix((6.0, -4.0, 8.0), (12.0, -8.0, 16.0), (18.0, -12.0, 24.0)))
  }

  test("Range") {
    assert(DenseVector.range(0, 10) == DenseVector(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    assert(norm(DenseVector.rangeD(0, 1, 0.1) - DenseVector(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) < 1e-10)
    assert(norm(DenseVector.rangeF(0f, 1f, 0.1f) - DenseVector(0.0f, 0.1f, 0.2f, 0.3f, 0.4f, 0.5f, 0.6f, 0.7f, 0.8f, 0.9f)) < 1e-6)
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
    val s2: DenseVector[Double] = x.slice(2, 4)

    assert(s === DenseVector(3.0, 4.0))
    assert(s2 === DenseVector(3.0, 4.0))

    val t = s.t

    assert(t === DenseVector(3.0, 4.0).t)

    val emptySlice = x(2 until 2)
    val emptySlice2 = x.slice(2, 2)
    assert(emptySlice === DenseVector[Double]())
    assert(emptySlice2 === DenseVector[Double]())
  }

  test("DenseVector * DenseMatrix Lifted OpMulMatrix") {
    val x = DenseVector[Int](1, 2, 3)
    val m = DenseMatrix((1, 2, 3), (2, 4, 6), (3, 6, 9))
    val mr = DenseMatrix((1, 2, 3))

    val xxt = x * x.t
    assert(xxt === m)

    val xm = x * mr
    assert(xm === m)

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

  test("Transpose Complex") {
    val x = DenseVector(Complex(1, 1), Complex(1, -1))
    val y = x.t
    val expected = new DenseMatrix(1, 2, Array(Complex(1, -1), Complex(1, 1)))
    assert(y === expected)
  }

  test("Transpose Apply") {
    val x = DenseVector(1, 2, 3)
    val xt = x.t
    assert(xt(1) === 2)
  }

  test("Map(Active)Pairs Double") {
    val a: DenseVector[Double] = DenseVector(1, 2, 3, 4, 5)
    val mv: DenseVector[Double] = a.mapPairs((i, x) => x + 1)
    val mav: DenseVector[Double] = a.mapActivePairs((i, x) => x + 1)
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

  test("Strided Map(Active)Values Double") {
    val a: DenseVector[Double] = DenseVector(1, 2, 3, 4, 5)
    val mv: DenseVector[Double] = a(2 to -1).mapValues(_ + 1)
    val mav: DenseVector[Double] = a(2 to -1).mapActiveValues(_ + 1)
    assert(mv === DenseVector(4.0, 5.0, 6.0))
    assert(mav === DenseVector(4.0, 5.0, 6.0))
  }

  test("Map(Active)Pairs Int") {
    val a: DenseVector[Int] = DenseVector(1, 2, 3, 4, 5)
    val mv: DenseVector[Int] = a.mapPairs((i, x) => x + 1)
    val mav: DenseVector[Int] = a.mapActivePairs((i, x) => x + 1)
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
    val mv: DenseVector[Float] = a.mapPairs((i, x) => x + 1)
    val mav: DenseVector[Float] = a.mapActivePairs((i, x) => x + 1)
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
    val a: DenseVector[Complex] = DenseVector(Complex(1, 1), Complex(2, 2))
    val mv: DenseVector[Complex] = a.mapValues(_ + Complex(1, 1))
    val mav: DenseVector[Complex] = a.mapActiveValues(_ + Complex(1, 1))
    assert(mv === DenseVector(Complex(2, 2), Complex(3, 3)))
    assert(mav === DenseVector(Complex(2, 2), Complex(3, 3)))
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
    val res = DenseMatrix((1.0, 2.0), (2.0, 3.0), (3.0, 4.0))
    assert(DenseVector.horzcat(a1, a2) === res)
  }

  test("Negation") {
    val a1 = DenseVector(1.0, 2.0, 3.0)
    assert(-a1 == DenseVector(-1.0, -2.0, -3.0))

  }

  test("Negation Tranpose") {
    val a1 = DenseVector(1.0, 2.0, 3.0)
    assert(-a1.t == DenseVector(-1.0, -2.0, -3.0).t)

  }

  test("DV ops work as Vector") {
    val a = DenseVector(1.0, 2.0, 3.0)
    val b = DenseVector(3.0, 4.0, 5.0)
    (a: Vector[Double]) += (b: Vector[Double])
    assert(a === DenseVector(4.0, 6.0, 8.0))
    assert((a: Vector[Double]).dot(b: Vector[Double]) === (a.dot(b)))
    (a: Vector[Double]) *= (b: Vector[Double])
    assert(a === DenseVector(12.0, 24.0, 40.0))
    a += (b: Vector[Double])
    assert(a === DenseVector(15.0, 28.0, 45.0))
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

  test("Vector <op> Vector should ensure vectors are same size") {
    {
      val a = Vector[Double](1D, 2D, 3D)
      val b = Vector[Double](1D, 2D, 3D, 4D)
      intercept[IllegalArgumentException] {
        a + b
      }
      intercept[IllegalArgumentException] {
        b + a
      }
    }
    {
      val a = Vector[Int](1, 2, 3)
      val b = Vector[Int](1, 2, 3, 4)
      intercept[IllegalArgumentException] {
        a + b
      }
      intercept[IllegalArgumentException] {
        b + a
      }
    }
  }

  test("Complex OpSet") {
    val a = DenseVector(Complex(1, 1))
    val b = DenseVector(Complex(2, 2))
    a := b
    assert(a === b)
  }

  test("Equals") {
    val a, b = DenseVector(3, 4, 5, 6)
    assert(a === b)
    a(0) = -1
    assert(a.slice(1, a.length) === b.slice(1, b.length))
    a.slice(0, 3) := b.slice(1, b.length)
    assert(a.slice(0, 3) === b.slice(1, b.length))
    a.slice(0, 4, 2) := b(0 until 2)
    assert(a.slice(0, 4, 2) === b(0 until 2))
  }

  test("toArray") {
    val a = DenseVector(1, 2, 3)
    assert(util.Arrays.equals(a.toArray, a.data))
    assert(util.Arrays.equals(a(0 until 3 by 2).toArray, Array(1, 3)))
    assert(util.Arrays.equals(a(1 until 3 by 1).toArray, Array(2, 3)))

    val b = DenseVector(1d * breeze.math.i, 0d * breeze.math.i, 2d * breeze.math.i).toArray
    //assert( util.Arrays.equals( b.toArray, Array(1d*breeze.math.i, 0d, 2d)) )
    assert(b(0) == Complex(0, 1))
    assert(b(1) == Complex(0, 0))
    assert(b(2) == Complex(0, 2))
  }

  test("OpEq and friends") {
    val a = DenseVector(1, 2, 3)
    val b = DenseVector(1, 4, 1)
    assert((a :== b) === BitVector(true, false, false))
    assert((a :!= b) === BitVector(false, true, true))
    assert((a <:= b) === BitVector(true, true, false))
    assert((a >:= b) === BitVector(true, false, true))
    assert((a <:< b) === BitVector(false, true, false))
    assert((a >:> b) === BitVector(false, false, true))
  }

  test("clip") {
    val dv = DenseVector.range(0, 10)
    assert(clip(dv, 1, 8) === DenseVector(1, 1, 2, 3, 4, 5, 6, 7, 8, 8))
    clip.inPlace(dv, 1, 8)
    assert(dv === DenseVector(1, 1, 2, 3, 4, 5, 6, 7, 8, 8))
  }

  test("clip tranpose") {
    val dv = DenseVector.range(0, 10)
    assert(clip(dv.t, 1, 8) === DenseVector(1, 1, 2, 3, 4, 5, 6, 7, 8, 8).t)
    clip.inPlace(dv.t, 1, 8)
    assert(dv.t === DenseVector(1, 1, 2, 3, 4, 5, 6, 7, 8, 8).t)
  }

  test("any and all") {
    val a = DenseVector(1, 2, 3)
    val b = DenseVector(1, 4, 1)
    assert(any(a :== b))
    assert(!all(a :== b))
    assert(any(a :== a))
    assert(all(a :== a))
    assert(!any(a :== (b - 1)))
  }

  test("boolean op") {
    val a = DenseVector(true, false, false)
    val b = DenseVector(true, true, false)

    assert((a &:& b) === DenseVector(true, false, false))
    assert((a |:| b) === DenseVector(true, true, false))
    assert((a ^^:^^ b) === DenseVector(false, true, false))
    assert(!a === DenseVector(false, true, true))
  }

  // blas causes me so many headaches
  test("negative step sizes and dot -- Double") {
    val foo = DenseVector(1.0, 2.0, 3.0, 4.0)
    val fneg = foo(3 to 0 by -1)
    assert((foo.dot(foo(3 to 0 by -1))) === 20.0)
  }

  test("negative step sizes and + -- Double") {
    val foo = DenseVector(1.0, 2.0, 3.0, 4.0)
    val fneg = foo(3 to 0 by -1)
    assert(foo + fneg === DenseVector(5.0, 5.0, 5.0, 5.0))
  }

  test("negative step sizes and scale -- Double") {
    val foo = DenseVector(1.0, 2.0, 3.0, 4.0)
    val fneg = foo(3 to 0 by -1)
    fneg *= 1.0
    assert(fneg * 3.0 === DenseVector(12.0, 9.0, 6.0, 3.0))
  }

  test("negative step sizes and assignment -- Double") {
    val foo = DenseVector(1.0, 2.0, 3.0, 4.0)
    val fneg = foo(3 to 0 by -1)
    fneg.copy
    val fy = DenseVector.zeros[Double](fneg.length)
    fy := fneg
    assert(fy === DenseVector(4.0, 3.0, 2.0, 1.0))
  }

  implicit def genTriple: Arbitrary[DenseVector[Double]] = Arbitrary {
    Arbitrary.arbitrary[Double].map(DenseVector.rand[Double](30) * _)
  }

  test("isClose") {
    check((a: DenseVector[Double]) => isClose(a, a))
    check((a: DenseVector[Double], b: DenseVector[Double]) =>
      isClose(a, b) == zipValues(a, b).forall((a, b) => (a - b).abs < 1E-8))
  }

  test("nonfinite") {
    check((a: DenseVector[Double]) => any(isNonfinite, a) == a.exists(isNonfinite(_)))
    check((a: DenseVector[Double]) => all(isNonfinite, a) == a.forall(isNonfinite(_)))
    assert(all(isNonfinite, DenseVector[Double]())(all.reduceUFunc))
    assert(!any(isNonfinite, DenseVector[Double]()))
  }

  test("#467 can slice transpose") {
    assert(DenseVector(3, 4).t(0 until 1) == DenseVector(3).t)
  }

  test("#669 rangeD off by one") {
    assert(DenseVector.rangeD(0.0, 10.0, 3.0) == DenseVector(0.0, 3.0, 6.0, 9.0))
    check { (_a: Int, _b: Int, _step: Int) =>
      val a = clipInt(_a)
      val b = clipInt(_b)
      val step = clipInt(_step) + 1
      try {
        DenseVector((a until b by step).map(_.toDouble): _*) == DenseVector.rangeD(a, b, step)
      } catch {
        // don't want to deal with the requirements
        case ex: IllegalArgumentException => true
      }
    }
  }

  test("#669 rangeF off by one") {
    assert(DenseVector.rangeF(0.0f, 10.0f, 3.0f) == DenseVector(0.0f, 3.0f, 6.0f, 9.0f))
    check { (_a: Int, _b: Int, _step: Int) =>
      val a = clipInt(_a)
      val b = clipInt(_b)
      val step = clipInt(_step) + 1
      try {
        DenseVector((a until b by step).map(_.toFloat): _*) == DenseVector.rangeF(a, b, step)
      } catch {
        // don't want to deal with the requirements
        case ex: IllegalArgumentException => true
      }
    }
  }

  private def clipInt(a: Int): Int = {
    if (a == Int.MaxValue) 0 else (a + 1).abs % 20
  }

  test("#751 rangeD/rangeF floor/ceiling issue") {
    val dvd = DenseVector.rangeD(0.9, 1.1, 0.1)
    val dvf = DenseVector.rangeF(0.9f, 1.1f, 0.1f)

    assert(dvd == DenseVector(0.9, 1.0))
    assert(dvf == DenseVector(0.9f, 1.0f))
  }

  test("#765 overlapping spans") {
    val a = DenseVector(1,2,3,4)
    a(1 to 3) := a(0 to 2)
    assert(a == DenseVector(1, 1, 2, 3))
  }

  test("#723 - non-unit stride slices") {
    val array=Array.range(0,16)
    val dv = new DenseVector[Int](array)
    for (i <- 0 until 4) {
      val fromNew = new DenseVector[Int](array, i, 4, 4)
      val slice = dv.slice(i, dv.length, 4)
      assert(fromNew === slice)
    }
  }

  test("mapping ufunc, strides") {
    val r = DenseVector.rand(100).apply(10 until 27)
    var explicit = new DenseVector(r.data.map(math.sin))
    explicit = explicit(10 until 27)
    assert(sin(r) == explicit)
    sin.inPlace(r)
    assert(explicit == r)
  }
}

abstract class DenseVectorPropertyTestBase[T: ClassTag] extends TensorSpaceTestBase[DenseVector[T], Int, T] {
  def genScalar: Arbitrary[T]

  override implicit def genSingle: Arbitrary[DenseVector[T]] = Arbitrary {
    Gen.choose(1, 10).flatMap(RandomInstanceSupport.genDenseVector(_, genScalar.arbitrary))
  }

  implicit def genTriple: Arbitrary[(DenseVector[T], DenseVector[T], DenseVector[T])] = Arbitrary {
    Gen.choose(1, 10).flatMap { n =>
      for {
        x <- RandomInstanceSupport.genDenseVector(n, genScalar.arbitrary)
        y <- RandomInstanceSupport.genDenseVector(n, genScalar.arbitrary)
        z <- RandomInstanceSupport.genDenseVector(n, genScalar.arbitrary)
      } yield (x, y, z)
    }
  }
}

/**
 *
 * @author dlwh
 */

class DenseVectorOps_DoubleTest
    extends DenseVectorPropertyTestBase[Double]
    with DoubleValuedTensorSpaceTestBase[DenseVector[Double], Int] {
  val space = DenseVector.space[Double]
  def genScalar: Arbitrary[Double] = RandomInstanceSupport.genReasonableDouble
}


class DenseVectorOps_IntTest extends DenseVectorPropertyTestBase[Int] {
  val space = DenseVector.space[Int]

  def genScalar: Arbitrary[Int] = Arbitrary { Gen.Choose.chooseInt.choose(-1000, 1000) }
}


class DenseVectorOps_ComplexTest extends DenseVectorPropertyTestBase[Complex] {
  val space = DenseVector.space[Complex]
  def genScalar: Arbitrary[Complex] = Arbitrary {
    for {
      r <- RandomInstanceSupport.genReasonableDouble.arbitrary
      i <- RandomInstanceSupport.genReasonableDouble.arbitrary
    } yield Complex(r, i)
  }
}


class DenseVectorOps_FloatTest extends DenseVectorPropertyTestBase[Float] {
  val space = DenseVector.space[Float]

  override val TOL: Double = 5E-3
  def genScalar: Arbitrary[Float] = RandomInstanceSupport.genReasonableFloat
}
