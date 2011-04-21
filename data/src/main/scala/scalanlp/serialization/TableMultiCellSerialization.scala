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
 * Reads a fixed number of cells from a table.
 *
 * @author dramage
 */
trait TableMultiCellReadable[@specialized V] extends Readable[TableRowReader,V] {
  def size : Int;
}

/**
 * Low priority conversions of cell readable to multi-cell readable.
 *
 * @author dramage
 */
trait LowPriorityTableMultiCellReadableImplicits {
  implicit def anyTableCellReadable[V](implicit rc : TableCellReadable[V])
  : TableMultiCellReadable[V] = new TableMultiCellReadable[V] {
    override def size = 1;
    override def read(row : TableRowReader) = {
      val rv = rc.read(row.next);
      require(!row.hasNext, "Wrong number of cells in row.");
      rv;
    }
  }
}

object TableMultiCellReadable extends LowPriorityTableMultiCellReadableImplicits {
  type Input = TableRowReader;

  implicit def forTuple2[A,B]
  (implicit ra : TableMultiCellReadable[A], rb : TableMultiCellReadable[B])
  : TableMultiCellReadable[(A,B)] = new TableMultiCellReadable[(A,B)] {
    override def size = 2;
    override def read(row : Input) =
      (ra.read(row.take(ra.size)),
       rb.read(row.take(rb.size)));
  }

  implicit def forTuple3[A,B,C]
  (implicit ra : TableMultiCellReadable[A], rb : TableMultiCellReadable[B],
   rc : TableMultiCellReadable[C])
  : TableMultiCellReadable[(A,B,C)] = new TableMultiCellReadable[(A,B,C)] {
    override def size = 3;
    override def read(row : Input) =
      (ra.read(row.take(ra.size)),
       rb.read(row.take(rb.size)),
       rc.read(row.take(rc.size)));
  }

  implicit def forTuple4[A,B,C,D]
  (implicit ra : TableMultiCellReadable[A], rb : TableMultiCellReadable[B],
   rc : TableMultiCellReadable[C], rd : TableMultiCellReadable[D])
  : TableMultiCellReadable[(A,B,C,D)] = new TableMultiCellReadable[(A,B,C,D)] {
    override def size = 4;
    override def read(row : Input) =
      (ra.read(row.take(ra.size)),
       rb.read(row.take(rb.size)),
       rc.read(row.take(rc.size)),
       rd.read(row.take(rd.size)));
  }

  implicit def forTuple5[A,B,C,D,E]
  (implicit ra : TableMultiCellReadable[A], rb : TableMultiCellReadable[B],
   rc : TableMultiCellReadable[C], rd : TableMultiCellReadable[D], re : TableMultiCellReadable[E])
  : TableMultiCellReadable[(A,B,C,D,E)] = new TableMultiCellReadable[(A,B,C,D,E)] {
    override def size = 5;
    override def read(row : Input) =
      (ra.read(row.take(ra.size)),
       rb.read(row.take(rb.size)),
       rc.read(row.take(rc.size)),
       rd.read(row.take(rd.size)),
       re.read(row.take(re.size)));
  }
}


/**
 * For writing a row of a table.
 *
 * @author dramage
 */
trait TableMultiCellWritable[V] extends Writable[TableRowWriter, V] {
  def size : Int;
}

/**
 * Low priority Writable conversions.
 *
 * @author dramage
 */
trait LowPriorityTableMultiCellWritableImplicits {
  implicit def anyTableCellWritable[V](implicit wc : TableCellWritable[V])
  : TableMultiCellWritable[V] = new TableMultiCellWritable[V] {
    override def size = 1;
    def write(writer : TableRowWriter, value : V) = {
      wc.write(writer.next, value);
    }
  }
}

object TableMultiCellWritable extends LowPriorityTableMultiCellWritableImplicits {
  type Output = TableRowWriter

  implicit def forTuple2[A,B]
  (implicit wa : TableMultiCellWritable[A],
   wb : TableMultiCellWritable[B])
  : TableMultiCellWritable[(A,B)] = new TableMultiCellWritable[(A,B)] {
    override def size = 2;
    def write(writer : Output, v : (A,B)) = {
      wa.write(writer, v._1);
      wb.write(writer, v._2);
    }
  }

  implicit def forTuple3[A,B,C]
  (implicit wa : TableMultiCellWritable[A],
   wb : TableMultiCellWritable[B],
   wc : TableMultiCellWritable[C])
  : TableMultiCellWritable[(A,B,C)] = new TableMultiCellWritable[(A,B,C)] {
    override def size = 3;
    def write(writer : Output, v : (A,B,C)) = {
      wa.write(writer, v._1);
      wb.write(writer, v._2);
      wc.write(writer, v._3);
    }
  }

  implicit def forTuple4[A,B,C,D]
  (implicit wa : TableMultiCellWritable[A],
   wb : TableMultiCellWritable[B],
   wc : TableMultiCellWritable[C],
   wd : TableMultiCellWritable[D])
  : TableMultiCellWritable[(A,B,C,D)] = new TableMultiCellWritable[(A,B,C,D)] {
    override def size = 4;
    def write(writer : Output, v : (A,B,C,D)) = {
      wa.write(writer, v._1);
      wb.write(writer, v._2);
      wc.write(writer, v._3);
      wd.write(writer, v._4);
    }
  }

  implicit def forTuple5[A,B,C,D,E]
  (implicit wa : TableMultiCellWritable[A],
   wb : TableMultiCellWritable[B],
   wc : TableMultiCellWritable[C],
   wd : TableMultiCellWritable[D],
   we : TableMultiCellWritable[E])
  : TableMultiCellWritable[(A,B,C,D,E)] = new TableMultiCellWritable[(A,B,C,D,E)] {
    override def size = 5;
    def write(writer : Output, v : (A,B,C,D,E)) = {
      wa.write(writer, v._1);
      wb.write(writer, v._2);
      wc.write(writer, v._3);
      wd.write(writer, v._4);
      we.write(writer, v._5);
    }
  }
}
