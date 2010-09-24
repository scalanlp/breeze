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
trait TableRowReader extends Iterator[TableCellReader] {
  override def take(n : Int) : TableRowReader = {
    val took = super.take(n);
    new TableRowReader {
      override def hasNext = took.hasNext;
      override def next = took.next;
    }
  }
}

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
trait TableRowReadable[V] extends Readable[TableRowReader,V] {
  /** Returns a header describing this row. */
  def header : Option[List[String]] = None;
}

/**
 * Low priority conversions of cell readable to row readable.
 *
 * @author dramage
 */
trait LowPriorityTableRowReadableImplicits {
  implicit def anyTableMultiCellReadable[V](implicit rc : TableMultiCellReadable[V])
  : TableRowReadable[V] = new TableRowReadable[V] {
    override def read(row : TableRowReader) = {
      val rv = rc.read(row.take(rc.size));
      require(!row.hasNext, "Wrong number of cells in row.");
      rv;
    }
  }
}

object TableRowReadable extends LowPriorityTableRowReadableImplicits {
  type Input = TableRowReader;

  implicit def forTuple2[A,B]
  (implicit ra : TableMultiCellReadable[A], rb : TableRowReadable[B])
  : TableRowReadable[(A,B)] = new TableRowReadable[(A,B)] {
    override def read(row : Input) =
      (ra.read(row.take(ra.size)),
       rb.read(row));
  }

  implicit def forTuple3[A,B,C]
  (implicit ra : TableMultiCellReadable[A], rb : TableMultiCellReadable[B],
   rc : TableRowReadable[C])
  : TableRowReadable[(A,B,C)] = new TableRowReadable[(A,B,C)] {
    override def read(row : Input) =
      (ra.read(row.take(ra.size)),
       rb.read(row.take(rb.size)),
       rc.read(row));
  }

  implicit def forTuple4[A,B,C,D]
  (implicit ra : TableMultiCellReadable[A], rb : TableMultiCellReadable[B],
   rc : TableMultiCellReadable[C], rd : TableRowReadable[D])
  : TableRowReadable[(A,B,C,D)] = new TableRowReadable[(A,B,C,D)] {
    override def read(row : Input) =
      (ra.read(row.take(ra.size)),
       rb.read(row.take(rb.size)),
       rc.read(row.take(rc.size)),
       rd.read(row));
  }

  implicit def forTuple5[A,B,C,D,E]
  (implicit ra : TableMultiCellReadable[A], rb : TableMultiCellReadable[B],
   rc : TableMultiCellReadable[C], rd : TableMultiCellReadable[D], re : TableRowReadable[E])
  : TableRowReadable[(A,B,C,D,E)] = new TableRowReadable[(A,B,C,D,E)] {
    override def read(row : Input) =
      (ra.read(row.take(ra.size)),
       rb.read(row.take(rb.size)),
       rc.read(row.take(rc.size)),
       rd.read(row.take(rd.size)),
       re.read(row));
  }

  implicit def forArray[A](implicit cr : TableMultiCellReadable[A], cm : ClassManifest[A])
  : TableRowReadable[Array[A]] = new TableRowReadable[Array[A]] {
    override def read(row : Input) = {
      val builder = scala.collection.mutable.ArrayBuilder.make[A];
      while (row.hasNext) {
        builder += cr.read(row.take(cr.size));
      }
      builder.result;
    }
  }

  implicit def forIterable[A](implicit cr : TableMultiCellReadable[A])
  : TableRowReadable[Iterable[A]] = new TableRowReadable[Iterable[A]] {
    override def read(row : Input) = {
      val builder = Iterable.newBuilder[A];
      while (row.hasNext) {
        builder += cr.read(row.take(cr.size));
      }
      builder.result;
    }
  }

  implicit def forList[A](implicit cr : TableMultiCellReadable[A])
  : TableRowReadable[List[A]] = new TableRowReadable[List[A]] {
    override def read(row : Input) = {
      val builder = List.newBuilder[A];
      while (row.hasNext) {
        builder += cr.read(row.take(cr.size));
      }
      builder.result;
    }
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
trait TableRowWritable[V] extends Writable[TableRowWriter, V] {
  /** Returns a header describing this row. */
  def header : Option[List[String]] = None;
}

/**
 * Low priority Writable conversions.
 *
 * @author dramage
 */
trait LowPriorityTableRowWritableImplicits {
  implicit def anyTableMultiCellWritable[V](implicit wc : TableMultiCellWritable[V])
  : TableRowWritable[V] = new TableRowWritable[V] {
    def write(writer : TableRowWriter, value : V) = {
      wc.write(writer, value);
      writer.finish;
    }
  }
}

object TableRowWritable extends LowPriorityTableRowWritableImplicits {
  type Output = TableRowWriter

  implicit def forTuple2[A,B]
  (implicit wa : TableMultiCellWritable[A],
   wb : TableRowWritable[B])
  : TableRowWritable[(A,B)] = new TableRowWritable[(A,B)] {
    def write(writer : Output, v : (A,B)) = {
      wa.write(writer, v._1);
      wb.write(writer, v._2);
    }
  }

  implicit def forTuple3[A,B,C]
  (implicit wa : TableMultiCellWritable[A],
   wb : TableMultiCellWritable[B],
   wc : TableRowWritable[C])
  : TableRowWritable[(A,B,C)] = new TableRowWritable[(A,B,C)] {
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
   wd : TableRowWritable[D])
  : TableRowWritable[(A,B,C,D)] = new TableRowWritable[(A,B,C,D)] {
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
   we : TableRowWritable[E])
  : TableRowWritable[(A,B,C,D,E)] = new TableRowWritable[(A,B,C,D,E)] {
    def write(writer : Output, v : (A,B,C,D,E)) = {
      wa.write(writer, v._1);
      wb.write(writer, v._2);
      wc.write(writer, v._3);
      wd.write(writer, v._4);
      we.write(writer, v._5);
    }
  }

  implicit def forArray[A:TableMultiCellWritable]
  : TableRowWritable[Array[A]] = new TableRowWritable[Array[A]] {
    override def write(writer : Output, coll : Array[A]) = {
      for (v <- coll) {
        implicitly[TableMultiCellWritable[A]].write(writer, v);
      }
      writer.finish;
    }
  }

  implicit def forIterable[A:TableMultiCellWritable]
  : TableRowWritable[Iterable[A]] = new TableRowWritable[Iterable[A]] {
    override def write(writer : Output, coll : Iterable[A]) = {
      for (v <- coll) {
        implicitly[TableMultiCellWritable[A]].write(writer, v);
      }
      writer.finish;
    }
  }

  implicit def forTraversable[A:TableMultiCellWritable]
  : TableRowWritable[Traversable[A]] = new TableRowWritable[Traversable[A]] {
    override def write(writer : Output, coll : Traversable[A]) = {
      for (v <- coll) {
        implicitly[TableMultiCellWritable[A]].write(writer, v);
      }
      writer.finish;
    }
  }

  implicit def forList[A:TableMultiCellWritable]
  : TableRowWritable[List[A]] = new TableRowWritable[List[A]] {
    override def write(writer : Output, coll : List[A]) = {
      for (v <- coll) {
        implicitly[TableMultiCellWritable[A]].write(writer, v);
      }
      writer.finish;
    }
  }
}

/**
 * A trait for companion objects to case classes that want to support
 * reading and writing table headers.
 *
 * Example:
 *
 * case class MyRow(id : String, count : Int, values : Array[Double]);
 * object MyRow extends TableRowCompanion[MyRow,(String,Int,Array[Double])];
 *
 * @author dramage
 */
trait TableRowCompanion[This,Format] { self =>
  import scalanlp.util.CanPack;
  import com.thoughtworks.paranamer.BytecodeReadingParanamer;

  private val method = try {
    this.getClass.getMethods.filter(_.getName == "apply").head;
  } catch {
    case ex : Throwable =>
      throw new IllegalArgumentException("No apply method.");
  }

  private val names : List[String] =
    new com.thoughtworks.paranamer.BytecodeReadingParanamer().lookupParameterNames(method).toList;

  class CompanionReadable(implicit trr : TableRowReadable[Format], cp : CanPack[Format])
  extends TableRowReadable[This] {
    override def header = Some(names);

    override def read(in : TableRowReader) = {
      val packed = implicitly[TableRowReadable[Format]].read(in);
      val unpacked = implicitly[CanPack[Format]].unpack(packed);
      method.invoke(null, unpacked.asInstanceOf[List[Object]].toArray[Object] :_*).asInstanceOf[This];
    }
  }

  private var _readable : CompanionReadable = null;
  implicit def readable(implicit trr : TableRowReadable[Format], cp : CanPack[Format]) : TableRowReadable[This] = {
    if (_readable == null) synchronized { _readable = new CompanionReadable(); }
    _readable;
  }

  class CompanionWritable(implicit trw : TableRowWritable[Format], cp : CanPack[Format], cm : Manifest[This])
  extends TableRowWritable[This] {
    override def header = Some(names);

    override def write(out : TableRowWriter, value : This) = {
      val unpacked : List[Any] =
        names.map(name => implicitly[Manifest[This]].erasure.getMethod(name).invoke(value));
      val packed = implicitly[CanPack[Format]].pack(unpacked);
      implicitly[TableRowWritable[Format]].write(out, packed);
    }
  }

  private var _writable : CompanionWritable = null;
  implicit def writable(implicit trw : TableRowWritable[Format], cp : CanPack[Format], cm : Manifest[This]) : TableRowWritable[This] = {
    if (_writable == null) synchronized { _writable = new CompanionWritable(); }
    _writable;
  }
}
