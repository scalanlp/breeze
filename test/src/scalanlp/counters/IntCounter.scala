package scalanlp.counters;
import org.scalacheck._;
import org.scalatest.prop._;
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._

object IntCounterSpecification extends PropSuite with Checkers {
  implicit def arbitraryCounter[T](implicit a : Arbitrary[T]) : Arbitrary[IntCounter[T]] = Arbitrary{ for(x <- Gen.listOf(a.arbitrary)) yield Counters.count(x); };

  test("clear", (c : IntCounter[Int])  => {c.clear(); c.size == 0 && c.total == 0});
  test("sumClear", (c : IntCounter[Int], c2 : IntCounter[Int])  => {c += c2; c.clear(); c.size == 0 && c.total == 0});
  test("sum", (c : IntCounter[Int], c2 : IntCounter[Int])  => {val expTotal = c.total + c2.total; val maxSize = c.size + c2.size; c+=c2; expTotal == c.total && c.size <= maxSize});
}
