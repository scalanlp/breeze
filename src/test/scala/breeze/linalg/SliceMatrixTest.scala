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

 }
