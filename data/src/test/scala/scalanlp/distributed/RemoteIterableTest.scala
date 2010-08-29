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

package scalanlp.distributed;

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class RemoteIterableTest extends FunSuite with Checkers {

  test("Remote iterable") {
    val service = RemoteIterable.service(List(1,2,3,4));
    service.runAsDaemon;

    val client = RemoteIterable.client[Int](service.uri);
    assert(client.iterator.isInstanceOf[RemoteIterator.Client[_]]);
    assert(client.toList === List(1,2,3,4));

    assert(client.map(_.toString).iterator.isInstanceOf[RemoteIterator.Client[_]]);
    assert(client.map(_.toString).toList === List("1","2","3","4"));

    service.stop;
  }

}
