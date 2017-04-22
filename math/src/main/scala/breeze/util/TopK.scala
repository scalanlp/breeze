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
package breeze.util

import java.util.TreeSet
import scala.collection.JavaConverters._

/**
 * A Top-K queue keeps a list of the top K elements seen so far as ordered
 * by the given comparator.
 */
class TopK[T](k: Int)(implicit ord: Ordering[T]) extends Iterable[T] {

  private val keys = new TreeSet[T](ord)

  def +=(e: T) = {
    if (keys.size < k) {
      keys.add(e)
    } else if (keys.size > 0 && ord.lt(keys.first, e) && !keys.contains(e)) {
      keys.remove(keys.first)
      keys.add(e)
    }
  }

  override def iterator: Iterator[T] =
    keys.descendingIterator.asScala

  override def size =
    keys.size
}

object TopK {
  def apply[T](k: Int, items: TraversableOnce[T])(implicit ord: Ordering[T]): TopK[T] = {
    val topk = new TopK[T](k)(ord)
    items.foreach(topk += _)
    topk
  }

  def apply[T, U](k: Int, items: TraversableOnce[T], scoreFn: T => U)(implicit uord: Ordering[U]): TopK[T] = {
    implicit val ord = new Ordering[T] {
      override def compare(x: T, y: T) = uord.compare(scoreFn(x), scoreFn(y))
    }
    apply(k, items)(ord)
  }
}

/**
 * A rich iterable extension that adds the topk method.
 */
class TopKIterable[T](val self: Iterable[T]) {
  def topk(k: Int)(implicit ord: Ordering[T]): TopK[T] =
    TopK(k, self)

  def topk[U](k: Int, scoreFn: T => U)(implicit uord: Ordering[U]): TopK[T] =
    TopK(k, self, scoreFn)(uord)
}

class TopKIterator[T](val self: Iterator[T]) {
  def topk(k: Int)(implicit ord: Ordering[T]): TopK[T] =
    TopK(k, self)

  def topk[U](k: Int, scoreFn: T => U)(implicit uord: Ordering[U]): TopK[T] =
    TopK(k, self, scoreFn)(uord)
}

object TopKImplicits {
  implicit def iTopKIterable[T](iterable: Iterable[T]) =
    new TopKIterable(iterable)

  implicit def iTopKIterator[T](iterator: Iterator[T]) =
    new TopKIterator(iterator)
}
