/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package scalanlp.optimize

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class KuhnMunkresTest extends FunSuite with Checkers {

  test("sanity check") {
    val arr = Array( Seq(2.,4.,7.,9.), Seq(3.,9.,5.,1.), Seq(8.,2.,9.,7.));
    val (matching,weight) = KuhnMunkres.extractMatching(arr.map(_.toSeq));
    assert(weight === 5.0);
    assert(matching(0) === 0)
    assert(matching(1) === 3)
    assert(matching(2) === 1)
  }

  test("another test") {
    val arr = Array ( Seq(14., 5., 8., 7),
                      Seq(1.5, 12., 6., 5),
                      Seq(7., 8., 3., 9),
                      Seq(2., 4., 6., 10) );
    val (matching,weight) = KuhnMunkres.extractMatching(arr.map(_.toSeq));
    println(matching);
    assert(weight === 15.0);
    assert(matching(0) === 1)
    assert(matching(1) === 3)
    assert(matching(2) === 2)
    assert(matching(3) === 0)
  }

}
