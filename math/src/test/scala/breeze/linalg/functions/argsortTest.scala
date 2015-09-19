package breeze.linalg.functions

import breeze.linalg.{DenseVector, argsort}
import org.scalacheck.Prop
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
 * Created by dlwh on 9/18/15.
 */
class argsortTest extends FunSuite with Checkers {
  test("argsort dv") {
    check(Prop.forAll{ (array: Array[Double]) =>
      val ax = argsort(new DenseVector(array))
      ax.toIndexedSeq.map(array) == array.sorted.toIndexedSeq
    })
  }

}
