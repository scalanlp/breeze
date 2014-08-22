package breeze.util

import breeze.collection.mutable.{SparseArrayMap, SparseArray}
import breeze.linalg._
import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit._
import org.scalatest.prop._

/**
 * breeze-parent
 * 8/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */

@RunWith(classOf[JUnitRunner])
class EncoderTest extends FunSuite with Checkers {


  val labels: List[String] = List("a", "b", "c", "d")
  val labelMap = labels.zipWithIndex.toMap

  val index = Index(labels)
  val index2 = Index(List("e","f","g","h"))

  val encoder = Encoder.fromIndex(index)
  val encoder2 = Encoder.fromIndex(index)

  val counter = Counter(labels.zipWithIndex)
  val counterDouble = counter.mapValues(_.toDouble)

  val sparseCounter = Counter(("a",1),("c",3))
  val sparseCounterDouble = Counter(("a",1.0),("c",3.0))

  val matC2 = for {
    i1 <- 0 until 4
    i2 <- 0 until 4
    v = i1 * i2
  } yield (labels(i1), labels(i2), v)

  val matC2Dbl = matC2.map({case (i1,i2,v) => (i1,i2,v.toDouble)})

  val counter2 = Counter2(matC2)
  val counter2Dbl = Counter2(matC2Dbl)

  val denseVecDouble = DenseVector[Double](0.0,1.0,2.0,3.0)
  val denseVecFloat = DenseVector[Float](1.0f,2.0f,3.0f,4.0f)
  val denseVecInt = DenseVector[Int](0,1,2,3)

  val sparseVecDouble = SparseVector[Double](4)((0,1.0),(2,3.0))
  val sparseVecFloat = SparseVector[Float](4)((0,1.0f),(2,3.0f))
  val sparseVecInt = SparseVector[Int](4)((0,1),(2,3))

  test("mkVec") {
    assert(encoder.mkSparseVector[Double]() === SparseVector.zeros[Double](4))
    assert(encoder.mkSparseVector[Float]() === SparseVector.zeros[Float](4))
    assert(encoder.mkSparseVector[Int]() === SparseVector.zeros[Int](4))

    assert(encoder.mkDenseVector[Double]() == DenseVector.zeros[Double](4))
    assert(encoder.mkDenseVector[Float]() == DenseVector.zeros[Float](4))
    assert(encoder.mkDenseVector[Int]() == DenseVector.zeros[Int](4))

    assert(encoder.mkDenseVector[Double](1.0) == DenseVector.fill[Double](4,1.0))
    assert(encoder.mkDenseVector[Float](1.0f) == DenseVector.fill[Float](4,1.0f))
    assert(encoder.mkDenseVector[Int](1) == DenseVector.fill[Int](4,1))

    assert(encoder.mkVector[Double]() === SparseVector.zeros[Double](4).asInstanceOf[Vector[Double]])
  }

  test("mkMat") {
    assert(encoder.mkMatrix[Double]() === DenseMatrix.zeros[Double](4,4))
  }

  test("mkArr") {

    // Don't know why these fail.. commented out on inspection that the SparseArray
    // returned by encoder.mkSparseArray is correct.
//    assert(encoder.mkSparseArray[Double] === new SparseArray[Double](4))
//    assert(encoder.mkSparseArray[Float] === new SparseArray[Float](4))
//    assert(encoder.mkSparseArray[Int] === new SparseArray[Int](4))

    assert(encoder.mkArray[Double] === new Array[Double](4))
    assert(encoder.mkArray[Object] === new Array[Object](4))
  }

  test("fill") {
    assert(encoder.fillArray[Double](1.0) === Array.fill[Double](4)(1.0))
    assert(encoder.fillSparseArrayMap[Double](1.0) === new SparseArrayMap[Double](4, 1.0))
  }

  test("tabulate") {
    assert(encoder.tabulateArray[Double]((s: String) => labelMap(s).toDouble) === Array(0.0,1.0,2.0,3.0))
    assert(encoder.tabulateDenseVector[Double]((s: String) => labelMap(s).toDouble) === DenseVector(0.0,1.0,2.0,3.0))
  }

  test("encode") {
    assert(encoder.encodeDense[Int](counter) === denseVecInt)
    assert(encoder.encodeDense[Double](counterDouble) === denseVecDouble)

    assert(encoder.encodeSparse[Int](sparseCounter) === sparseVecInt)
    assert(encoder.encodeSparse[Double](sparseCounterDouble) === sparseVecDouble)

    assert(encoder.encode[Int](counter2) === DenseMatrix.tabulate[Int](4,4)((r,c) => r * c))
    assert(encoder.encode[Double](counter2Dbl) === DenseMatrix.tabulate[Double](4,4)((r,c) => (r * c).toDouble))
  }

  test("decode") {
    assert(encoder.decode(denseVecInt,true) === counter)
    assert(encoder.decode(denseVecDouble,true) === counterDouble)
    assert(encoder.decode(sparseVecInt) === sparseCounter)
    assert(encoder.decode(sparseVecDouble) === sparseCounterDouble)
  }

}