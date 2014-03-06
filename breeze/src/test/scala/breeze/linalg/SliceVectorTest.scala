package breeze.linalg

import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
class SliceVectorTest extends FunSuite {

  test("basic slices of a counter") {
    val ctr = Counter('a -> 1, 'b -> 2, 'c -> 42)
    val v:Vector[Int] = ctr(Seq('c, 'b))
    assert(v(0) === 42)
    assert(v(1) === 2)

    v(0) = 10
    assert(ctr('c) === 10)
    ctr('b) = 1
    assert(v(1) === 1)
  }

  test("negative indexing tests"){

    val tempDV = DenseVector(0, 1, 2, 3, 4, 5, 6)
    assert( tempDV( 0 to 3 ) == DenseVector(0, 1, 2, 3), "Failed> tempDV( 0 to 3 ) = " + tempDV( 0 to 3 )  )
    assert( tempDV( 4 to -1 ) == DenseVector(4, 5, 6), "Failed> tempDV( 4 to -1 ) =  " + tempDV( 4 to -1 ) )
    assert( tempDV( -3 to -1 ) == DenseVector(4, 5, 6), "Failed> tempDV( -3 to -1 ) =  " + tempDV( -3 to -1 ) )
    //Following throws IllegalArgumentException, only positive end indexes for "until"
    //assert( tempDV( -3 until -1 ) == DenseVector(4, 5, 6), "Failed> tempDV( -3 until -1 ) =  " + tempDV( -3 until -1 ) )
    //Following works
    intercept[IllegalArgumentException]{ tempDV( -3 until 7 ) }
    assert( tempDV( 0 to 4 by 2 ) == DenseVector(0, 2, 4), "Failed> tempDV( 0 to 4 by 2 ) = " + tempDV( 0 to 4 by 2 ) )
    assert( tempDV( 4 to 0 by -2 ) == DenseVector(4, 2, 0), "Failed> tempDV( 4 to 0 by -2 ) = " + tempDV( 4 to 0 by -2 ) )
    assert( tempDV( -1 ) == 6, "Failed> tempDV( -1 ) = " + tempDV( -1 )  )
    assert( tempDV( -7 ) == 0, "Failed> tempDV( -7 ) = " + tempDV( -7 )  )

  }


}
