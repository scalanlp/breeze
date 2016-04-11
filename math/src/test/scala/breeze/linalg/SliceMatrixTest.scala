package breeze.linalg

import org.scalatest.FunSuite

/**
  *
  * @author dlwh
  */
class SliceMatrixTest extends FunSuite {

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
    val a = DenseMatrix.tabulate[Int](5,5){ (i, j) => 5 * i + j + 1}

    // first matrix with the 4th row and 3rd column removed
    val b = a(Seq(0, 1, 2, 4), Seq(0, 1, 3, 4))

    // check row slice
    assert(b(1, ::) == DenseVector(6, 7, 9, 10), "Failed> b(1, ::) = " + b(1, ::))

    // check column slice
    assert(b(::, 1) == DenseVector(2, 7, 12, 22), "Failed> b(::, 1) = " + b(::, 1))

    // check arb ro5w slice
    assert(b(Seq(1, 3), ::) ==  DenseMatrix.create(2, 4,  Array(6, 7, 9, 10, 21, 22, 24, 25), 0, 4, true), "Failed> b(Seq(1, 3), ::) = " + b(Seq(1,3), ::))

    // check arb col slice
    assert(b(::, Seq(1, 3)) == DenseMatrix.create( 4, 2,  Array(2, 7, 12, 22, 5, 10, 15, 25), 0, 4, false), "Failed> b(::, Seq(1,3) = " + b(Seq(1, 3), ::))



  }

 }
