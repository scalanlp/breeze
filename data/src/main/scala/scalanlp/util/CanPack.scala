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
package scalanlp.util

/**
 * Capability trait for packing and unpacking tuples (or other things) to a
 * List of Any.  This class is used by reflection based companion object
 * mix-ins, such as TableRowCompanion.
 *
 * @author dramage
 */
trait CanPack[T] {
  def unpack(value : T) : List[Any];
  def pack(values : List[Any]) : T;
}

/**
 * Low priority implicit for packing a single value as itself.
 *
 * @author
 */
trait LowPriorityCanPackImplicits {
  implicit def canPackValue[T] = new CanPack[T] {
    def unpack(value : T) = List[Any](value);
    def pack(values : List[Any]) = {
      require(values.length == 1, "Wrong size");
      values.head.asInstanceOf[T];
    }
  }
}

/**
 * Implicit packers for tuples.
 *
 * @author dramage
 */
object CanPack extends LowPriorityCanPackImplicits {
  implicit def canPackTuple2[A,B] : CanPack[(A,B)] = new CanPack[(A,B)] {
    def unpack(value : (A,B)) = List[Any](value._1, value._2);
    def pack(values : List[Any]) = {
      require(values.length == 2, "Wrong size");
      val iter = values.iterator;
      (iter.next, iter.next).asInstanceOf[(A,B)];
    }
  }

  implicit def canPackTuple3[A,B,C] : CanPack[(A,B,C)] = new CanPack[(A,B,C)] {
    def unpack(value : (A,B,C)) = List[Any](value._1, value._2, value._3);
    def pack(values : List[Any]) = {
      require(values.length == 3, "Wrong size");
      val iter = values.iterator;
      (iter.next, iter.next, iter.next).asInstanceOf[(A,B,C)];
    }
  }

  implicit def canPackTuple4[A,B,C,D] : CanPack[(A,B,C,D)] = new CanPack[(A,B,C,D)] {
    def unpack(value : (A,B,C,D)) = List[Any](value._1, value._2, value._3, value._4);
    def pack(values : List[Any]) = {
      require(values.length == 4, "Wrong size");
      val iter = values.iterator;
      (iter.next, iter.next, iter.next, iter.next).asInstanceOf[(A,B,C,D)];
    }
  }

  implicit def canPackTuple5[A,B,C,D,E] : CanPack[(A,B,C,D,E)] = new CanPack[(A,B,C,D,E)] {
    def unpack(value : (A,B,C,D,E)) = List[Any](value._1, value._2, value._3, value._4, value._5);
    def pack(values : List[Any]) = {
      require(values.length == 5, "Wrong size");
      val iter = values.iterator;
      (iter.next, iter.next, iter.next, iter.next, iter.next).asInstanceOf[(A,B,C,D,E)];
    }
  }

  implicit def canPackTuple6[A,B,C,D,E,F] : CanPack[(A,B,C,D,E,F)] = new CanPack[(A,B,C,D,E,F)] {
    def unpack(value : (A,B,C,D,E,F)) = List[Any](value._1, value._2, value._3, value._4, value._5, value._6);
    def pack(values : List[Any]) = {
      require(values.length == 6, "Wrong size");
      val iter = values.iterator;
      (iter.next, iter.next, iter.next, iter.next, iter.next, iter.next).asInstanceOf[(A,B,C,D,E,F)];
    }
  }

  implicit def canPackTuple7[A,B,C,D,E,F,G] : CanPack[(A,B,C,D,E,F,G)] = new CanPack[(A,B,C,D,E,F,G)] {
    def unpack(value : (A,B,C,D,E,F,G)) = List[Any](value._1, value._2, value._3, value._4, value._5, value._6, value._7);
    def pack(values : List[Any]) = {
      require(values.length == 7, "Wrong size");
      val iter = values.iterator;
      (iter.next, iter.next, iter.next, iter.next, iter.next, iter.next, iter.next).asInstanceOf[(A,B,C,D,E,F,G)];
    }
  }
}
