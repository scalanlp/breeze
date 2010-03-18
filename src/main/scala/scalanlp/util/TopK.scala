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
package scalanlp.util;

import scala.collection.mutable.ArrayBuffer;

/**
 * A Top-K queue keeps a list of the top K elements seen so far as ordered
 * by the given comparator.
 */
class TopK[T](k : Int)(implicit ord : Ordering[T]) extends Iterable[T] {

  final val keys = new ArrayBuffer[T](k+1);

  private[util] def += (elem : T) : Unit = {
    val pos = insertion(elem);
    keys.insert(if (pos < 0) ~pos else pos, elem);
    if (keys.size > k) {
      keys.remove(0);
    }
  }

  override def iterator : Iterator[T] = {
    keys.view.reverse.iterator
  }

  override def size =
    keys.size;

  @inline final def insertion(key : T) : Int = {
    if (keys.length == 0) return -1;

    var begin = 0;
    var end = keys.length - 1;
    var mid = (end + begin) >> 1;

    while (begin <= end) {
      mid = (end + begin) >> 1;
      val cmp = ord.compare(key, keys(mid));
      if (cmp > 0)
        begin = mid + 1;
      else if (cmp < 0)
        end = mid - 1;
      else
        return mid;
    }

    // no match found, return insertion point
    if (ord.lteq(key, keys(mid)))
      return -(mid)-1;     // Insert here (before mid)
    else
      return -(mid + 1)-1; // Insert after mid
  }
}

object TopK {
  def apply[T](k : Int, items : Iterator[T])(implicit ord : Ordering[T]) : TopK[T] = {
    val topk = new TopK[T](k);
    items.foreach(topk.+=);
    topk;
  }

  def apply[T,U](k : Int, items : Iterator[T], scoreFn : (T => U))(implicit uord : Ordering[U]) : TopK[T] = {
    implicit val ord = new Ordering[T] {
      override def compare(x : T, y : T) = uord.compare(scoreFn(x), scoreFn(y));
    };
    apply(k, items)(ord);
  }

  def apply[T](k : Int, items : Iterable[T])(implicit ord : Ordering[T]) : TopK[T] =
    this.apply(k, items.iterator)(ord);

  def apply[T,U](k : Int, items : Iterable[T], scoreFn : (T => U))(implicit uord : Ordering[U]) : TopK[T] =
    this.apply(k, items.iterator, scoreFn)(uord);
}

/**
 * A rich iterable extension that adds the topk method.
 */
class TopKIterable[T](val self : Iterable[T]) {
  def topk(k : Int)(implicit ord : Ordering[T]) : TopK[T] =
    TopK(k, self);

  def topk[U](k : Int, scoreFn : (T => U))(implicit uord : Ordering[U]) : TopK[T] =
    TopK(k, self, scoreFn)(uord);
}

class TopKIterator[T](val self : Iterator[T]) {
  def topk(k : Int)(implicit ord : Ordering[T]) : TopK[T] =
    TopK(k, self);

  def topk[U](k : Int, scoreFn : (T => U))(implicit uord : Ordering[U]) : TopK[T] =
    TopK(k, self, scoreFn)(uord);
}

object TopKImplicits {
  implicit def iTopKIterable[T](iterable : Iterable[T]) =
    new TopKIterable(iterable);

  implicit def iTopKIterator[T](iterator : Iterator[T]) =
    new TopKIterator(iterator);
}
