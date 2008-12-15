package scalanlp.math;
import org.scalacheck._;
import org.scalatest.prop._;
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object LogDoubleSpecification extends PropSuite with Checkers {

  test("add", (d:Double, e:Double)  => { (new LogDouble(d) + new LogDouble(e)).value == d + e})
}
