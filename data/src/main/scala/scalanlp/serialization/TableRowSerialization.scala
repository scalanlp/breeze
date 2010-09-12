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

package scalanlp;
package serialization;

/**
 * Reads a row as a series of readers.  Note that the returned readers cannot
 * be cached or accessed out of order, because they are a view on a single
 * underlying stream.
 *
 * @author dramage
 */
trait TableRowReader extends Iterator[TableCellReader];

object TableRowReader {
  implicit def fromStrings(strings : Iterable[String]) : TableRowReader = {
    val iter = strings.iterator;
    new TableRowReader {
      override def hasNext = iter.hasNext;
      override def next = TableCellReader.fromString(iter.next);
    }
  }
}

/**
 * Reads a row in a table.
 * 
 * @author dramage
 */
trait TableRowReadable[V] extends Readable[TableRowReader,V];

/**
 * Low priority conversions of cell readable to row readable.
 *
 * @author dramage
 */
trait LowPriorityTableRowReadableImplicits {
  implicit def anyTableCellReadable[V](implicit rc : TableCellReadable[V])
  : TableRowReadable[V] = new TableRowReadable[V] {
    override def read(row : TableRowReader) = {
      val rv = rc.read(row.next);
      require(!row.hasNext, "Wrong number of cells in row.");
      rv;
    }
  }
}

object TableRowReadable extends LowPriorityTableRowReadableImplicits {
  type Input = TableRowReader;

  implicit def forTuple2[A,B]
  (implicit ra : TableCellReadable[A], rb : TableRowReadable[B])
  : TableRowReadable[(A,B)] = new TableRowReadable[(A,B)] {
    override def read(row : Input) =
      (ra.read(row.next),
       rb.read(row));
  }

  implicit def forTuple3[A,B,C]
  (implicit ra : TableCellReadable[A], rb : TableCellReadable[B],
   rc : TableRowReadable[C])
  : TableRowReadable[(A,B,C)] = new TableRowReadable[(A,B,C)] {
    override def read(row : Input) =
      (ra.read(row.next),
       rb.read(row.next),
       rc.read(row));
  }

  implicit def forTuple4[A,B,C,D]
  (implicit ra : TableCellReadable[A], rb : TableCellReadable[B],
   rc : TableCellReadable[C], rd : TableRowReadable[D])
  : TableRowReadable[(A,B,C,D)] = new TableRowReadable[(A,B,C,D)] {
    override def read(row : Input) =
      (ra.read(row.next),
       rb.read(row.next),
       rc.read(row.next),
       rd.read(row));
  }

  implicit def forTuple5[A,B,C,D,E]
  (implicit ra : TableCellReadable[A], rb : TableCellReadable[B],
   rc : TableCellReadable[C], rd : TableCellReadable[D], re : TableRowReadable[E])
  : TableRowReadable[(A,B,C,D,E)] = new TableRowReadable[(A,B,C,D,E)] {
    override def read(row : Input) =
      (ra.read(row.next),
       rb.read(row.next),
       rc.read(row.next),
       rd.read(row.next),
       re.read(row));
  }

  implicit def forArray[A:TableCellReadable:ClassManifest]
  : TableRowReadable[Array[A]] = new TableRowReadable[Array[A]] {
    override def read(row : Input) = {
      row.map(implicitly[TableCellReadable[A]].read).toArray
    }
  }

  implicit def forIterable[A:TableCellReadable]
  : TableRowReadable[Iterable[A]] = new TableRowReadable[Iterable[A]] {
    override def read(row : Input) =
      row.map(implicitly[TableCellReadable[A]].read).toIterable;
  }

  implicit def forList[A:TableCellReadable]
  : TableRowReadable[List[A]] = new TableRowReadable[List[A]] {
    override def read(row : Input) =
      row.map(implicitly[TableCellReadable[A]].read).toList;
  }
}


/**
 * Writes a delimited row to output.  Call next before writing
 * to each cell.  Then call finish when done with the row.
 *
 * @author dramage
 */
trait TableRowWriter {
  def next() : TableCellWriter;
  def finish();
}

/**
 * For writing a row of a table.
 *
 * @author dramage
 */
trait TableRowWritable[V] extends Writable[TableRowWriter, V];

/**
 * Low priority Writable conversions.
 *
 * @author dramage
 */
trait LowPriorityTableRowWritableImplicits {
  implicit def anyTableCellWritable[V](implicit wc : TableCellWritable[V])
  : TableRowWritable[V] = new TableRowWritable[V] {
    def write(writer : TableRowWriter, value : V) = {
      wc.write(writer.next, value);
      writer.finish;
    }
  }
}

object TableRowWritable extends LowPriorityTableRowWritableImplicits {
  type Output = TableRowWriter

  implicit def forTuple2[A,B]
  (implicit wa : TableCellWritable[A],
   wb : TableRowWritable[B])
  : TableRowWritable[(A,B)] = new TableRowWritable[(A,B)] {
    def write(writer : Output, v : (A,B)) = {
      wa.write(writer.next, v._1);
      wb.write(writer, v._2);
    }
  }

  implicit def forTuple3[A,B,C]
  (implicit wa : TableCellWritable[A],
   wb : TableCellWritable[B],
   wc : TableRowWritable[C])
  : TableRowWritable[(A,B,C)] = new TableRowWritable[(A,B,C)] {
    def write(writer : Output, v : (A,B,C)) = {
      wa.write(writer.next, v._1);
      wb.write(writer.next, v._2);
      wc.write(writer, v._3);
    }
  }

  implicit def forTuple4[A,B,C,D]
  (implicit wa : TableCellWritable[A],
   wb : TableCellWritable[B],
   wc : TableCellWritable[C],
   wd : TableRowWritable[D])
  : TableRowWritable[(A,B,C,D)] = new TableRowWritable[(A,B,C,D)] {
    def write(writer : Output, v : (A,B,C,D)) = {
      wa.write(writer.next, v._1);
      wb.write(writer.next, v._2);
      wc.write(writer.next, v._3);
      wd.write(writer, v._4);
    }
  }

  implicit def forTuple5[A,B,C,D,E]
  (implicit wa : TableCellWritable[A],
   wb : TableCellWritable[B],
   wc : TableCellWritable[C],
   wd : TableCellWritable[D],
   we : TableRowWritable[E])
  : TableRowWritable[(A,B,C,D,E)] = new TableRowWritable[(A,B,C,D,E)] {
    def write(writer : Output, v : (A,B,C,D,E)) = {
      wa.write(writer.next, v._1);
      wb.write(writer.next, v._2);
      wc.write(writer.next, v._3);
      wd.write(writer.next, v._4);
      we.write(writer, v._5);
    }
  }

  implicit def forArray[A:TableCellWritable]
  : TableRowWritable[Array[A]] = new TableRowWritable[Array[A]] {
    override def write(writer : Output, coll : Array[A]) = {
      for (v <- coll) {
        implicitly[TableCellWritable[A]].write(writer.next, v);
      }
      writer.finish;
    }
  }

  implicit def forIterable[A:TableCellWritable]
  : TableRowWritable[Iterable[A]] = new TableRowWritable[Iterable[A]] {
    override def write(writer : Output, coll : Iterable[A]) = {
      for (v <- coll) {
        implicitly[TableCellWritable[A]].write(writer.next, v);
      }
      writer.finish;
    }
  }

  implicit def forTraversable[A:TableCellWritable]
  : TableRowWritable[Traversable[A]] = new TableRowWritable[Traversable[A]] {
    override def write(writer : Output, coll : Traversable[A]) = {
      for (v <- coll) {
        implicitly[TableCellWritable[A]].write(writer.next, v);
      }
      writer.finish;
    }
  }

  implicit def forList[A:TableCellWritable]
  : TableRowWritable[List[A]] = new TableRowWritable[List[A]] {
    override def write(writer : Output, coll : List[A]) = {
      for (v <- coll) {
        implicitly[TableCellWritable[A]].write(writer.next, v);
      }
      writer.finish;
    }
  }
}


/**
 * Serialization format for reading and writing rows of a table.
 *
 * @author dramage
 */
object TableRowSerialization {
  type Input = TableRowReader;
  type Output = TableRowWriter;

  type Readable[V] = TableRowReadable[V];
  type Writable[V] = TableRowWritable[V];
}
