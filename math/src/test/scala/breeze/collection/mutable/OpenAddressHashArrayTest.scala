package breeze.collection.mutable

import org.scalatest.prop.Checkers
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalacheck.{Arbitrary, Prop}

/**
 *
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class OpenAddressHashArrayTest extends FunSuite with Checkers {
  test("add stuff, make sure it and only it is there") {
    implicit val arbInt:Arbitrary[Int] = Arbitrary{Arbitrary.arbInt.arbitrary.map(_.abs % 1000)}
    check{Prop.forAll(  (data: List[Int], size: Int) =>
      size <= 0 || {
        val mdata = data.map( i => math.abs(i) % size).filter(_ >= 0).toSet
        val arr = new OpenAddressHashArray[Int](size)
        assert(arr.size > 0, size)
        for(i <- mdata) {
          arr(i) = i
        }
        mdata.forall(i => arr(i) == i) && !(0 until size).filterNot(mdata).exists(arr(_) != 0)
      }
    )}
  }

}
