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
  }

 }
