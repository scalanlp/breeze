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
class CompetitiveLinkingTest extends FunSuite with Checkers {
  test("sanity check") {
    val arr = Array( Seq(2.,4.,7.,9.), Seq(3.,9.,5.,1.), Seq(8.,2.,9.,7.));
    val (matching,weight) = CompetitiveLinking.extractMatching(arr.map(_.toSeq));
    assert(weight === 5.0);
    assert(matching(0) === 0)
    assert(matching(1) === 3)
    assert(matching(2) === 1)
  }

}
