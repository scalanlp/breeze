package scalanlp.collection.mutable;

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


import scala.collection.mutable.PriorityQueue;
import scala.collection.generic.BuilderFactory;

/**
 * Represents a beam, which is essentially a priority queue
 * with a maximum size.
 * 
 * @author dlwh
 */
class Beam[T](val maxSize:Int, xs:T*)(implicit o : Ordering[T]) { outer =>
  assert(maxSize >= 0)
  private val queue = new PriorityQueue[T]()(o.reverse);
  queue ++= xs;

  def size = queue.size;

  def map[U](f: T=>U)(implicit oU: Ordering[U]) = {
    val q = new PriorityQueue[U]()(oU.reverse);
    for(t <- queue) q += f(t);
    new Beam[U](maxSize) {
      queue ++= q;
    }
  }

  def flatMap[U](f: T=>Iterable[U])(implicit oU: Ordering[U]) = new Beam[U](maxSize) {
      for(x <- outer.queue;
          y <- f(x)) {
        if(queue.size < maxSize) {
          queue += y;
        } else if (queue.max < y) { // q.max is the smallest element.
          queue.dequeue();
          queue += y;
        }
      }
  }

  def filter[U](f: T=>Boolean) = new Beam[T](maxSize)  {
    queue ++= outer.queue.filter(f);
  }

  def +(x:T) = {this += x; this}
  def ++(x:Iterator[T]) = {this ++= x; this};
  def ++(x:Iterable[T]) = {this ++= x; this};

  def +=(x:T) = { cat(queue,x); }
  def ++=(x:Iterator[T]) = { x.foreach{cat(queue,_)}}
  def ++=(x:Iterable[T]) = { x.foreach(cat(queue,_))}

  private def trim() {
    while(queue.size > maxSize) {
      queue.dequeue();
    }
  }

  private def cat(h : PriorityQueue[T], x : T) {
    if(h.size < maxSize) h += x;
    else if (h.max < x) {h.dequeue(); h += x;}
  }

  def iterator = queue.iterator;
  override def toString() = iterator.mkString("Beam(",",",")");

}

object Beam {
  def apply[T](maxSize:Int, xs:T*)(implicit ordering: Ordering[T]) = new Beam[T](maxSize, xs:_*);
}
