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
 * Reads a cell in a table.
 *
 * @author dramage
 */
trait TableCellReader extends TextReader;

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
trait TableCellReadable[V] extends Readable[TableCellReader, V];

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
      if (in.read != -1) {
        // NB: this has to be an in.read to make sure we consume the
        // separator if reader only peeked at it
        throw new SerializationException("Reader not fully consume cell in row.");
      }
      rv;
    }
  }
}

object TableCellReadable extends LowPriorityTableCellReadableImplicits {
  implicit object forString extends TableCellReadable[String] {
    override def read(in : TableCellReader) =
      in.readRemaining;
  }
}

/**
 * Writes a delimited cell to output.  Call finishCell when done with
 * the cell.
 *
 * @author dramage
 */
trait TableCellWriter extends TextWriter {
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
trait TableCellWritable[V] extends Writable[TableCellWriter, V];

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
}


/**
 * Serialization format for reading and writing cells in a table.
 *
 * @author dramage
 */
object TableCellSerialization {

  type Input = TableCellReader;
  type Output = TableCellWriter;

  type Readable[T] = TableCellReadable[T];
  type Writable[T] = TableCellWritable[T];

  /** Marshalls the given value as a string. */
  def toString[T:Writable](value: T) : String = {
    val sb = new StringBuilder();
    implicitly[Writable[T]].write(sb, value);
    sb.toString;
  }

  /** Demarshalls a value from the given string. */
  def fromString[T:Readable](str: String) : T = {
    implicitly[Readable[T]].read(new TextReader.StringReader(str) with TableCellReader);
  }
}
