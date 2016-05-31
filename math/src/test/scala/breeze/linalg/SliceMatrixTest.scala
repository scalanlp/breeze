package breeze.linalg

import org.scalatest.FunSuite

/**
  *
  * @author dlwh
  */
class SliceMatrixTest extends FunSuite {
  val originalMatrix = DenseMatrix.tabulate[Int](5,5){ (i, j) => 5 * i + j + 1}

  // first matrix with the 4th row and 3rd column removed
  val sliceMatrix = originalMatrix(Seq(0, 1, 2, 4), Seq(0, 1, 3, 4))

  test("basic slices of a counter2") {
     val ctr = Counter2( ('a,0, 1), ('b, 1, 10), ('a,1, 6))
     val v:Matrix[Int] = ctr(Seq('a, 'b), Seq(1, 0))
     assert(v(0,0) === ctr('a,1))
     assert(v(0,1) === ctr('a,0))
     assert(v(1,1) === ctr('b,0))

     v(1,1) = 100
     assert(ctr('b,0) === 100)
     ctr('b, 1) = 1
     assert(v(1,0) === 1)
   }

  test("negative indexing tests"){

    val tempDM = DenseMatrix.tabulate(5, 5)( (x: Int, y: Int) => x + y*10)

    assert( tempDM( 0 to 1, 3 ) == DenseVector(30, 31), "Failed> tempDM( 0 to 1, 3 ) = " + tempDM( 0 to 1, 3 )  )
    assert( tempDM( 3 to -1, 3 ) == DenseVector(33, 34), "Failed> tempDM( 3 to -1, 3 ) =  " + tempDM( 3 to -1, 3 ) )
    assert( tempDM( -2 to -1, 3 ) == DenseVector(33, 34), "Failed> tempDM( -2 to -1, 3 ) =  " +tempDM( -2 to -1, 3 ) )
    intercept[IllegalArgumentException]{ tempDM( -2 until 5, 3 ) }
    assert( tempDM( -2, 3 ) == 33, "Failed> tempDM( -2, 3 ) =  " + tempDM( -2, 3 ) )
    assert( tempDM( 0, -5 ) == 0, "Failed> tempDM( 0, -5 ) =  " + tempDM( 0, -5 ) )
  }

  test("operations on slices"){
    val a = DenseMatrix.ones[Double](5,5)
    val b = DenseMatrix.ones[Double](5,5)
    val indices = Seq(0,1)

    val expected = DenseMatrix.ones[Double](5,5)

    for (row <- indices; col <- indices) {
      expected(row, col) += 1
    }

    val as = a(indices, indices)
    val bs = b(indices, indices)

    as += bs

    assert(expected.equals(a), "Failed to execute the addition on the slices")
  }

  test("slices of a slice matrix") {
    // check row slice
    assert(sliceMatrix(1, ::) == DenseVector(6, 7, 9, 10).t, "Failed> b(1, ::) = " + sliceMatrix(1, ::))

    // check column slice
    assert(sliceMatrix(::, 1) == DenseVector(2, 7, 12, 22), "Failed> b(::, 1) = " + sliceMatrix(::, 1))

    // check arb row slice
    assert(sliceMatrix(Seq(1, 3), ::) == DenseMatrix.create(2, 4,  Array(6, 7, 9, 10, 21, 22, 24, 25), 0, 4, true), "Failed> b(Seq(1, 3), ::) = " + sliceMatrix(Seq(1,3), ::))

    // check arb col slice
    assert(sliceMatrix(::, Seq(1, 3)) == DenseMatrix.create( 4, 2,  Array(2, 7, 12, 22, 5, 10, 15, 25), 0, 4, false), "Failed> b(::, Seq(1,3) = " + sliceMatrix(Seq(1, 3), ::))
  }

  test("canSliceRow") {
    assert(sliceMatrix(0, ::) == DenseVector( 1,  2,  4,  5).t, "Failed> sliceMatrix(0, ::) = " + sliceMatrix(0, ::))
    assert(sliceMatrix(1, ::) == DenseVector( 6,  7,  9, 10).t, "Failed> sliceMatrix(1, ::) = " + sliceMatrix(1, ::))
    assert(sliceMatrix(2, ::) == DenseVector(11, 12, 14, 15).t, "Failed> sliceMatrix(2, ::) = " + sliceMatrix(2, ::))
    assert(sliceMatrix(3, ::) == DenseVector(21, 22, 24, 25).t, "Failed> sliceMatrix(3, ::) = " + sliceMatrix(3, ::))
  }

  test("canSliceRowAndWeirdCols") {
    assert(sliceMatrix(0, Seq(0, 2)) == DenseVector( 1,  4).t, "Failed> sliceMatrix(0, ::) = " + sliceMatrix(0, Seq(0, 2)))
    assert(sliceMatrix(1, Seq(0, 2)) == DenseVector( 6,  9).t, "Failed> sliceMatrix(1, ::) = " + sliceMatrix(1, Seq(0, 2)))
    assert(sliceMatrix(2, Seq(0, 2)) == DenseVector(11,  14).t, "Failed> sliceMatrix(2, ::) = " + sliceMatrix(2, Seq(0, 2)))
    assert(sliceMatrix(3, Seq(0, 2)) == DenseVector(21,  24).t, "Failed> sliceMatrix(3, ::) = " + sliceMatrix(3, Seq(0, 2)))
  }

  test("canSliceCol") {
    assert(sliceMatrix(::, 0) == DenseVector( 1,  6, 11, 21), "Failed> sliceMatrix(::, 0) = " + sliceMatrix(::, 0))
    assert(sliceMatrix(::, 1) == DenseVector( 2,  7, 12, 22), "Failed> sliceMatrix(::, 1) = " + sliceMatrix(::, 1))
    assert(sliceMatrix(::, 2) == DenseVector( 4,  9, 14, 24), "Failed> sliceMatrix(::, 2) = " + sliceMatrix(::, 2))
    assert(sliceMatrix(::, 3) == DenseVector( 5, 10, 15, 25), "Failed> sliceMatrix(::, 3) = " + sliceMatrix(::, 3))
  }

  test("canSliceWeirdRowsAndCol") {
    assert(sliceMatrix(Seq(0, 2), 0) == DenseVector( 1, 11), "Failed> sliceMatrix(::, 0) = " + sliceMatrix(Seq(0, 2), 0))
    assert(sliceMatrix(Seq(0, 2), 1) == DenseVector( 2, 12), "Failed> sliceMatrix(::, 1) = " + sliceMatrix(Seq(0, 2), 1))
    assert(sliceMatrix(Seq(0, 2), 2) == DenseVector( 4, 14), "Failed> sliceMatrix(::, 2) = " + sliceMatrix(Seq(0, 2), 2))
    assert(sliceMatrix(Seq(0, 2), 3) == DenseVector( 5, 15), "Failed> sliceMatrix(::, 3) = " + sliceMatrix(Seq(0, 2), 3))
  }

  test("canSliceTensorBoolean") {
    val booleanTensor = BitVector(true, false, true, false)
    assert(sliceMatrix(booleanTensor, ::) == DenseMatrix.create(2, 4, Array(1, 2, 4, 5, 11, 12, 14, 15), 0, 4, true))

    assert(sliceMatrix(::, booleanTensor) == DenseMatrix.create(4, 2, Array(1, 6, 11, 21, 4, 9, 14, 24), 0, 4, false))

    assert(sliceMatrix(booleanTensor, booleanTensor) == DenseMatrix.create(2, 2, Array(1, 4, 11, 14), 0, 2, true))

    assert(sliceMatrix(booleanTensor, 0) == DenseVector(1, 11))
    assert(sliceMatrix(booleanTensor, 1) == DenseVector(2, 12))
    assert(sliceMatrix(booleanTensor, 2) == DenseVector(4, 14))
    assert(sliceMatrix(booleanTensor, 3) == DenseVector(5, 15))

    assert(sliceMatrix(0, booleanTensor) == DenseVector(1, 4).t)
    assert(sliceMatrix(1, booleanTensor) == DenseVector(6, 9).t)
    assert(sliceMatrix(2, booleanTensor) == DenseVector(11, 14).t)
    assert(sliceMatrix(3, booleanTensor) == DenseVector(21, 24).t)
  }
 }
