package scalanlp.counters;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

/*
import org.scalacheck._;
import org.scalatest.prop._;
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
object IntCounterTest extends PropSuite with Checkers {
  implicit def arbitraryCounter[T](implicit a : Arbitrary[T]) : Arbitrary[scalanlp.counters.IntCounter[T]] = Arbitrary{ for(x <- Gen.listOf(a.arbitrary)) yield Counters.count(x); };

  test("clear", (c : IntCounter[Int])  => {c.clear(); c.size == 0 && c.total == 0});
  test("sumClear", (c : IntCounter[Int], c2 : IntCounter[Int])  => {c += c2; c.clear(); c.size == 0 && c.total == 0});
  test("sum", (c : IntCounter[Int], c2 : IntCounter[Int])  => {val expTotal = c.total + c2.total; val maxSize = c.size + c2.size; c+=c2; expTotal == c.total && c.size <= maxSize});
}
*/
