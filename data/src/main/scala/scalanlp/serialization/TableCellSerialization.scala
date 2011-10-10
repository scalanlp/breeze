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

import scalanlp.io.{TextReader,TextWriter};

/**
 * Reads a cell in a table.
 *
 * @author dramage
 */
trait TableCellReader extends TextReader {
  override def close() =
    { /* do nothing */ }
    
  def finish() = {
    // NB: this has to be a read to make sure we consume the
    // separator if reader only peeked at it
    if (read != -1) {
      die("Reader did not fully consume cell in row");
    }
  }
}

object TableCellReader {
  implicit def fromString(string : String) =
    new StringReader(string);

  class StringReader(string : String) extends TextReader.StringReader(string) with TableCellReader;
}

/**
 * Readable for reading a table cell.
 * 
 * @author dramage
 */
trait TableCellReadable[@specialized V] extends Readable[TableCellReader, V];

/**
 * Low priority conversion of any to TextSerialization.Readable.
 *
 * @author dramage
 */
trait LowPriorityTableCellReadableImplicits {
  implicit def anyTextReadable[V](implicit tr : TextSerialization.Readable[V])
  : TableCellReadable[V] = new TableCellReadable[V] {
    override def read(in : TableCellReader) = {
      val rv = tr.read(in);
      in.finish();
      rv;
    }
  }
}

object TableCellReadable extends LowPriorityTableCellReadableImplicits {
  implicit object forUnit extends TableCellReadable[Unit] {
    override def read(in : TableCellReader) = {
      in.finish();
      ();
    }
  }

  implicit object forString extends TableCellReadable[String] {
    override def read(in : TableCellReader) = {
      val rv = in.readRemaining;
      in.finish;
      rv;
    }
  }

  implicit def forOption[V:TableCellReadable] : TableCellReadable[Option[V]] = new TableCellReadable[Option[V]] {
    override def read(in : TableCellReader) = {
      if (in.peek() == -1) {
        in.finish();
        None;
      } else {
        val rv = Some(implicitly[TableCellReadable[V]].read(in));
        in.finish();
        rv;
      }
    }
  }

  implicit def forOptionSome[V:TableCellReadable] : TableCellReadable[Some[V]] = new TableCellReadable[Some[V]] {
    override def read(in : TableCellReader) = {
      if (in.peek() == -1)
        in.die("Expected a value while parsing Some()");

      Some(implicitly[TableCellReadable[V]].read(in));
    }
  }
  
  /** Static reader for doubles. */
  implicit object forDouble extends TableCellReadable[Double] {
    val reader = implicitly[TextSerialization.Readable[Double]];
    override def read(in : TableCellReader) = {
      val rv = reader.read(in);
      in.finish();
      rv;
    }
  }
  
  /** Static reader for ints. */
  implicit object forInt extends TableCellReadable[Int] {
    val reader = implicitly[TextSerialization.Readable[Int]];
    override def read(in : TableCellReader) = {
      val rv = reader.read(in);
      in.finish();
      rv;
    }
  }
}

/**
 * Writes a delimited cell to output.  Call finishCell when done with
 * the cell.
 *
 * @author dramage
 */
trait TableCellWriter extends TextWriter {
  override def close() =
    { /* do nothing */ }
    
  def finish();
}

object TableCellWriter {
  implicit def fromStringBuilder(sb : StringBuilder) =
    new StringBuilderWriter(sb);

  class StringBuilderWriter(sb : StringBuilder) extends TextWriter.StringBuilderWriter(sb) with TableCellWriter {
    override def finish() {
      // do nothing
    };
  }
}

/**
 * Writable for writing a table cell.
 *
 * @author dramage
 */
trait TableCellWritable[@specialized V] extends Writable[TableCellWriter, V];

/**
 * Low priority conversion of any TextWritable.
 *
 * @author dramage
 */
trait LowPriorityTableCellWritableImplicits {
  implicit def anyTextWritable[V:TextSerialization.Writable]
  : TableCellWritable[V] = new TableCellWritable[V] {
    override def write(writer : TableCellWriter, value : V) = {
      implicitly[TextSerialization.Writable[V]].write(writer, value);
      writer.finish();
    }
  }
}

object TableCellWritable extends LowPriorityTableCellWritableImplicits {
  implicit object forString extends TableCellWritable[String] {
    override def write(writer : TableCellWriter, string : String) = {
      writer.append(string);
      writer.finish();
    }
  }

  implicit def forOption[V:TableCellWritable] : TableCellWritable[Option[V]] = new TableCellWritable[Option[V]] {
    override def write(writer : TableCellWriter, value : Option[V]) = {
      value match {
        case Some(v) => implicitly[TableCellWritable[V]].write(writer,v);
        case None => writer.finish;
      }
    }
  }

  implicit def forOptionSome[V:TableCellWritable] : TableCellWritable[Some[V]] = new TableCellWritable[Some[V]] {
    override def write(writer : TableCellWriter, value : Some[V]) =
      implicitly[TableCellWritable[V]].write(writer,value.get);
  }
  
  /** Static writer for doubles. */
  implicit object forDouble extends TableCellWritable[Double] {
    val fmt = implicitly[TextSerialization.Writable[Double]];
    override def write(writer : TableCellWriter, value : Double) = {
      fmt.write(writer, value);
      writer.finish();
    }
  }
  
  /** Static writer for ints. */
  implicit object forInt extends TableCellWritable[Int] {
    val fmt = implicitly[TextSerialization.Writable[Int]];
    override def write(writer : TableCellWriter, value : Int) = {
      fmt.write(writer, value);
      writer.finish();
    }
  }
}


/**
 * Serialization format for reading and writing cells in a table.
 *
 * @author dramage
 */
object TableCellSerialization {

  /** Marshalls the given value as a string. */
  def toString[T:TableCellWritable](value: T) : String = {
    val sb = new StringBuilder();
    implicitly[TableCellWritable[T]].write(sb, value);
    sb.toString;
  }

  /** Demarshalls a value from the given string. */
  def fromString[T:TableCellReadable](str: String) : T = {
    implicitly[TableCellReadable[T]].read(new TextReader.StringReader(str) with TableCellReader);
  }
}
