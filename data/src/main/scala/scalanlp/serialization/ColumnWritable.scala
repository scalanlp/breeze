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

package scalanlp.serialization;

/**
 * Format for an individual cell.
 */
trait CellWritable[-V] {
  def string(value : V) : String;
}
/**
 * Low priority ColumnWritable conversions.
 *
 * @author dramage
 */
trait LowPriorityCellWritableImplicits {
  implicit def anyTextWritable[V:TextSerialization.Writable]
  : CellWritable[V] = new CellWritable[V] {
    override def string(value : V) =
      TextSerialization.toString(value);
  }
}

object CellWritable extends LowPriorityCellWritableImplicits {
  implicit object forString extends CellWritable[String] {
    override def string(string : String) = string;
  }
}

/**
 * A rich iterable of sequences of strings that can be written to
 * a CSV or TSV file. See constructor methods in companion object.
 *
 * @author dramage
 */
trait ColumnWritable[-V] {
  /** Gets the strings for V to be formatted as columns. */
  def strings(value : V) : Iterator[String];
}

/**
 * Low priority ColumnWritable conversions.
 *
 * @author dramage
 */
trait LowPriorityColumnWritableImplicits {
  implicit def anyCellWritable[V:CellWritable]
  : ColumnWritable[V] = new ColumnWritable[V] {
    override def strings(value : V) = {
      Iterator.single(implicitly[CellWritable[V]].string(value));
    }
  }
}

object ColumnWritable extends LowPriorityColumnWritableImplicits {
  implicit def forTuple2[A,B]
  (implicit wa : CellWritable[A], wb : ColumnWritable[B])
  : ColumnWritable[(A,B)] = new ColumnWritable[(A,B)] {
    override def strings(v : (A,B)) = {
      Iterator(wa.string(v._1)) ++
      wb.strings(v._2);
    }
  }

  implicit def forTuple3[A,B,C]
  (implicit wa : CellWritable[A], wb : CellWritable[B], wc : ColumnWritable[C])
  : ColumnWritable[(A,B,C)] = new ColumnWritable[(A,B,C)] {
    override def strings(v : (A,B,C)) = {
      Iterator(wa.string(v._1), wb.string(v._2)) ++
      wc.strings(v._3);
    }
  }

  implicit def forTuple4[A,B,C,D]
  (implicit wa : CellWritable[A], wb : CellWritable[B], wc : CellWritable[C],
   wd : ColumnWritable[D])
  : ColumnWritable[(A,B,C,D)] = new ColumnWritable[(A,B,C,D)] {
    override def strings(v : (A,B,C,D)) = {
      Iterator(wa.string(v._1), wb.string(v._2), wc.string(v._3)) ++
      wd.strings(v._4);
    }
  }

  implicit def forTuple5[A,B,C,D,E]
  (implicit wa : CellWritable[A], wb : CellWritable[B], wc : CellWritable[C],
   wd : CellWritable[D], we : ColumnWritable[E])
  : ColumnWritable[(A,B,C,D,E)] = new ColumnWritable[(A,B,C,D,E)] {
    override def strings(v : (A,B,C,D,E)) = {
      Iterator(wa.string(v._1), wb.string(v._2), wc.string(v._3), wd.string(v._4)) ++
      we.strings(v._5);
    }
  }

  implicit def forArray[A:CellWritable]
  : ColumnWritable[Array[A]] = new ColumnWritable[Array[A]] {
    override def strings(v : Array[A]) = {
      v.iterator.map(implicitly[CellWritable[A]].string);
    }
  }

  implicit def forIterable[A:CellWritable]
  : ColumnWritable[Iterable[A]] = new ColumnWritable[Iterable[A]] {
    override def strings(v : Iterable[A]) = {
      v.iterator.map(implicitly[CellWritable[A]].string);
    }
  }
}
