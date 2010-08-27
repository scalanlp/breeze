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
 * Reads a cell value.
 *
 * @author dramage
 */
trait CellReadable[V] {
  def value(string : String) : V;
}

/**
 * Low priority conversion of any to TextSerialization.Readable to a
 * CellReadable.
 *
 * @author dramage
 */
trait LowPriorityCellReadableImplicits {
  implicit def anyTextReadable[V:TextSerialization.Readable]
  : CellReadable[V] = new CellReadable[V] {
    override def value(string : String) =
      TextSerialization.fromString(string);
  }
}

object CellReadable extends LowPriorityCellReadableImplicits {
  implicit object forString extends CellReadable[String] {
    override def value(string : String) = string;
  }
}

/**
 * Reads column data from a set of strings.
 *
 * @author dramage
 */
trait ColumnReadable[V] {
  def value(strings : Seq[String]) : V;
}

/**
 * Low priority conversion of any to CellReadable to a ColumnReadable
 * that expects only a single column.
 *
 * @author dramage
 */
trait LowPriorityColumnReadableImplicits {
  implicit def anyCellReadable[V:CellReadable]
  : ColumnReadable[V] = new ColumnReadable[V] {
    override def value(strings : Seq[String]) = {
      require(strings.length == 1, "Wrong number of cells in row.");
      implicitly[CellReadable[V]].value(strings.head);
    }
  }
}

object ColumnReadable extends LowPriorityColumnReadableImplicits {
  implicit def forTuple2[A,B]
  (implicit ra : CellReadable[A], rb : ColumnReadable[B])
  : ColumnReadable[(A,B)] = new ColumnReadable[(A,B)] {
    override def value(strings : Seq[String]) =
      (ra.value(strings.head),
       rb.value(strings.tail));
  }

  implicit def forTuple3[A,B,C]
  (implicit ra : CellReadable[A], rb : CellReadable[B],
   rc : ColumnReadable[C])
  : ColumnReadable[(A,B,C)] = new ColumnReadable[(A,B,C)] {
    override def value(strings : Seq[String]) =
      (ra.value(strings(0)),
       rb.value(strings(1)),
       rc.value(strings.drop(2)));
  }

  implicit def forTuple4[A,B,C,D]
  (implicit ra : CellReadable[A], rb : CellReadable[B],
   rc : CellReadable[C], rd : ColumnReadable[D])
  : ColumnReadable[(A,B,C,D)] = new ColumnReadable[(A,B,C,D)] {
    override def value(strings : Seq[String]) =
      (ra.value(strings(0)),
       rb.value(strings(1)),
       rc.value(strings(2)),
       rd.value(strings.drop(3)));
  }

  implicit def forTuple5[A,B,C,D,E]
  (implicit ra : CellReadable[A], rb : CellReadable[B],
   rc : CellReadable[C], rd : CellReadable[D], re : ColumnReadable[E])
  : ColumnReadable[(A,B,C,D,E)] = new ColumnReadable[(A,B,C,D,E)] {
    override def value(strings : Seq[String]) =
      (ra.value(strings(0)),
       rb.value(strings(1)),
       rc.value(strings(2)),
       rd.value(strings(3)),
       re.value(strings.drop(4)));
  }

  implicit def forArray[A:CellReadable:ClassManifest]
  : ColumnReadable[Array[A]] = new ColumnReadable[Array[A]] {
    override def value(strings : Seq[String]) =
      strings.view.map(implicitly[CellReadable[A]].value).toArray;
  }

  implicit def forIterable[A:CellReadable]
  : ColumnReadable[Iterable[A]] = new ColumnReadable[Iterable[A]] {
    override def value(strings : Seq[String]) =
      strings.view.map(implicitly[CellReadable[A]].value);
  }

  implicit def forList[A:CellReadable]
  : ColumnReadable[List[A]] = new ColumnReadable[List[A]] {
    override def value(strings : Seq[String]) =
      strings.view.map(implicitly[CellReadable[A]].value).toList;
  }
}
