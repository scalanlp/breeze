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

import java.io.File;
import scalanlp.ra.Cell;
import scalanlp.pipes.Pipes;

/**
 * Reads a table as a series of TableRowReader.  Note that the returned
 * readers cannot be cached or accessed out of order, because they are a
 * view on a single underlying stream.
 *
 * @author dramage
 */
trait TableReader extends Iterator[TableRowReader];

/**
 * Reads type V from a table.  V can be Iterator[R], Array[R], or
 * List[R] for any R that is itself TableRowReadable.
 *
 * @author dramage
 */
trait TableReadable[V] extends Readable[TableReader,V];

object TableReadable {
  implicit def toIterator[V:TableRowReadable]
  : TableReadable[Iterator[V]] = new TableReadable[Iterator[V]] {
    override def read(tr : TableReader) =
      tr.map(implicitly[TableRowReadable[V]].read);
  }

  implicit def toArray[V:ClassManifest:TableRowReadable]
  : TableReadable[Array[V]] = new TableReadable[Array[V]] {
    override def read(tr : TableReader) =
      tr.map(implicitly[TableRowReadable[V]].read).toArray;
  }

  implicit def toList[V:TableRowReadable]
  : TableReadable[List[V]] = new TableReadable[List[V]] {
    override def read(tr : TableReader) =
      tr.map(implicitly[TableRowReadable[V]].read).toList;
  }
}

/**
 * Writes a table to output.  Call next before writing each row.
 * Each row must be finished with a call to row.finish. Then call
 * table.finish when done with the table.
 *
 * @author dramage
 */
trait TableWriter {
  def next() : TableRowWriter;
  def finish();
}

/**
 * Writes type V to a table.  V can be Iterator[R] or Traversable[R]
 * for any R that is TableRowWritable.
 *
 * @author dramage
 */
trait TableWritable[V] extends Writable[TableWriter,V];

object TableWritable {
  implicit def forIterator[V:TableRowWritable]
  : TableWritable[Iterator[V]] = new TableWritable[Iterator[V]] {
    override def write(tw : TableWriter, vv : Iterator[V]) = {
      for (v <- vv) {
        implicitly[TableRowWritable[V]].write(tw.next(), v);
      }
      tw.finish();
    }
  }

  implicit def forTraversable[V:TableRowWritable]
  : TableWritable[Traversable[V]] = new TableWritable[Traversable[V]] {
    override def write(tw : TableWriter, vv : Traversable[V]) = {
      for (v <- vv) {
        implicitly[TableRowWritable[V]].write(tw.next(), v);
      }
      tw.finish();
    }
  }

  implicit def forIterable[V:TableRowWritable]
  : TableWritable[Iterable[V]] = new TableWritable[Iterable[V]] {
    override def write(tw : TableWriter, vv : Iterable[V]) = {
      for (v <- vv) {
        implicitly[TableRowWritable[V]].write(tw.next(), v);
      }
      tw.finish();
    }
  }

  implicit def forList[V:TableRowWritable]
  : TableWritable[List[V]] = new TableWritable[List[V]] {
    override def write(tw : TableWriter, vv : List[V]) = {
      for (v <- vv) {
        implicitly[TableRowWritable[V]].write(tw.next(), v);
      }
      tw.finish();
    }
  }

  implicit def forArray[V:TableRowWritable]
  : TableWritable[Array[V]] = new TableWritable[Array[V]] {
    override def write(tw : TableWriter, vv : Array[V]) = {
      for (v <- vv) {
        implicitly[TableRowWritable[V]].write(tw.next(), v);
      }
      tw.finish();
    }
  }
}

trait TextTableSerialization { self =>
  
  def read[V:TableReadable](source : TextReader) : V;

  def write[V:TableWritable](sink : TextWriter, value : V);

  /** Evaluates the given eval function if no cache exists; otherwise loads value. */
  def cache[V:TableReadable:TableWritable](path : File)(eval : => V) : V =
    new Cell(path, eval).get;

  def cache[V:TableReadable:TableWritable](name : String, pipes : Pipes = Pipes.global)(eval : => V) : V =
    new Cell(pipes.file(name), eval).get;

  implicit def fileReadWritable[V:TableReadable:TableWritable] : FileSerialization.ReadWritable[V] =
    new FileReadWritable[V];

  class FileReadWritable[V:TableReadable:TableWritable] extends FileSerialization.ReadWritable[V] {
    override def read(file : java.io.File) =
      self.read[V](file);
    override def write(file : java.io.File, value : V) =
      self.write(file, value);
  }
}

/**
 * Convenience methods for reading and writing TableReadables and
 * TableWritables to CSV files and streams.
 *
 * @author
 */
object CSVTableSerialization extends TextTableSerialization {
  type Input = TableReader;
  type Output = TableWriter;

  type Readable[V] = TableReadable[V];
  type Writable[V] = TableWritable[V];

  def read[V:TableReadable](source : TextReader) =
    implicitly[TableReadable[V]].read(new CSVTableReader(source));

  def write[V:TableWritable](sink : TextWriter, value : V) =
    implicitly[TableWritable[V]].write(new CSVTableWriter(sink), value);
}

/**
 * Convenience methods for reading and writing TableReadables and
 * TableWritables to TSV files and streams.
 *
 * @author
 */
object TSVTableSerialization extends TextTableSerialization {
  type Input = TableReader;
  type Output = TableWriter;

  type Readable[V] = TableReadable[V];
  type Writable[V] = TableWritable[V];

  def read[V:TableReadable](source : TextReader) =
    implicitly[TableReadable[V]].read(new TSVTableReader(source));

  def write[V:TableWritable](sink : TextWriter, value : V) =
    implicitly[TableWritable[V]].write(new TSVTableWriter(sink), value);
}

/**
 * Thrown if hasNext or next is called in a TableReader or
 * TableRowReader before the preceding row or cell has been fully consumed.
 *
 * @author dramage
 */
class TableAccessException(msg : String)
extends RuntimeException(msg);

/**
 * Thrown if encountering an error while writing, such as a character
 * that needs quoting in a writer that has no quotes.
 *
 * @author dramage
 */
class TableWriteException(msg : String)
extends RuntimeException


object TextTableSerialization {
  /** Constant used to signify that cells cannot be quoted. */
  val NoQuote = -2;

  type Input = TextTableReader;
  type Output = TextTableWriter;
}


/**
 * Thrown when encountering a problem during parsing a table.
 *
 * @author dramage
 */
class TextTableParseException(msg : String, cause : Throwable, lineNo : Int, colNo : Int)
extends TextReaderException(msg, cause, lineNo, colNo) {
  def this(msg : String, lineNo : Int, colNo : Int) =
    this(msg, null, lineNo, colNo);
}

/**
 * A reader for reading tables that follow CSV-like rules.  Fields
 * are separated with the given separator char.  Fields are quoted
 * with the given quote character, which may be TextTableSerialization.NoQuote
 * if fields cannot be quoted.
 *
 * <p>Note that the returned TableRowReaders cannot be cached or
 * accessed out of order, as they are a view on a single underlying stream.</p>
 *
 * @author dramage
 */
class TextTableReader
(val source : TextReader, val separator : Int, val quote : Int)
extends TableReader
{ self =>

  /** Row state: true if we are awaiting a new line start. */
  protected var awaitingLine = true;

  /** Row state: true if we are awaiting a new cell start. */
  protected var awaitingCell = false;

  /** Cell state: true if we are in a quoted cell. */
  protected var inQuotedCell = false;

  /** Cell state: true if we are in an unquoted cell. */
  protected var inRawCell = false;

  /** Cell state: true if we are in an empty cell. */
  protected var inEmptyCell = false;

  def hasNext = {
    if (!awaitingLine)
      throw new TableAccessException("hasNext called before previous TableRowReader complete");

    source.peek != -1;
  }

  def next = {
    if (!hasNext)
      throw new NoSuchElementException();

    awaitingLine = false;
    awaitingCell = true;
    RowReader;
  }

  object RowReader extends TableRowReader {
    def hasNext = {
      if (!awaitingLine && !awaitingCell) {
        throw new TableAccessException("hasNext called before previous TableCellReader complete");
      } else if (awaitingLine && awaitingCell) {
        throw new AssertionError("Unexpected table reader state - this is a bug in "+self.getClass);
      }
      
      awaitingCell;
    }

    def next = {
      if (!hasNext)
        throw new NoSuchElementException();

      val cp = source.peek;
      if (cp == quote) {
        // quoted cell
        source.read();
        awaitingCell = false;
        inQuotedCell = true;
      } else if (cp != separator && cp != '\r' && cp != '\n' && cp != -1) {
        // unquoted, non-empty cell
        awaitingCell = false;
        inRawCell = true;
      } else {
        // empty cell, possibly line-final
        awaitingCell = false;
        inEmptyCell = true;
      }

      CellReader;
    }


    object CellReader extends TableCellReader {
      override def columnNumber =
        source.columnNumber;

      override def lineNumber =
        source.lineNumber;

      def consumeEndCell() {
        if (awaitingLine || awaitingCell) {
          throw new AssertionError("Unexpected state in cell reader (0)");
        }
        val cp = source.peek();
        if (cp == separator) {
          source.read;
          awaitingCell = true;
          awaitingLine = false;
        } else if (cp == '\r' || cp == '\n') {
          source.readNewline;
          awaitingCell = false;
          awaitingLine = true;
        } else if (cp == -1) {
          awaitingCell = false;
          awaitingLine = true;
        } else {
          throw new TextTableParseException("Unexpected content", source.lineNumber, source.columnNumber);
        }
      }

      override def read() = {
        if (inQuotedCell) {
          val cp = source.peek();
          if (cp == quote) {
            source.read();
            // quote value
            if (source.peek == quote) {
              // escaped quote
              source.read();
            } else {
              // end of cell - waiting to consume
              inQuotedCell = false;
              consumeEndCell();
              -1;
            }
          } else if (cp == -1) {
            // unexpected end of file
            throw new TextTableParseException("Runaway quote", source.lineNumber, source.columnNumber);
          } else {
            // normal value
            source.read();
          }
        } else if (inRawCell) {
          val cp = source.peek();
          if (cp == quote) {
            // unexpected quote
            throw new TextTableParseException("Unexpected quote in unquoted cell", source.lineNumber, source.columnNumber);
          } else if (cp == separator || cp == '\r' || cp == '\n' || cp == -1) {
            // end of cell
            inRawCell = false;
            consumeEndCell();
            -1;
          } else {
            // normal value
            source.read();
          }
        } else if (inEmptyCell) {
          val cp = source.peek();
          if (cp == separator || cp == '\r' || cp == '\n' || cp == -1) {
            // end of cell
            inEmptyCell = false;
            consumeEndCell();
            -1;
          } else {
            throw new TextTableParseException("Unexpected contents in cell", source.lineNumber, source.columnNumber);
          }
        } else {
          throw new AssertionError("Unexpected table reader state - this is a bug in "+self.getClass);
        }
      }

      override def peek() = {
        if (inQuotedCell) {
          val cp = source.peek();
          if (cp == quote) {
            if (source.peek(1) == quote) {
              quote;
            } else {
              -1;
            }
          } else if (cp == '\r' || cp == '\n' || cp == -1) {
            // unexpected end of line or file
            throw new TextTableParseException("Runaway quote", source.lineNumber, source.columnNumber);
          } else {
            cp;
          }
        } else if (inRawCell) {
          val cp = source.peek();
          if (cp == quote) {
            // unexpected quote
            throw new TextTableParseException("Unexpected quote in raw cell", source.lineNumber, source.columnNumber);
          } else if (cp == separator || cp == '\r' || cp == '\n' || cp == -1) {
            // end of cell
            -1;
          } else {
            // normal value
            cp;
          }
        } else {
          -1;
        }
      }

      override def peek(n : Int) =
        if (n == 0) peek() else throw new IllegalArgumentException("Can only peek(0)");
    }
  }
}

/**
 * For reading CSV tables.
 *
 * @author dramage
 */
class CSVTableReader(source : TextReader)
extends TextTableReader(source, ',', '"');

/**
 * For reading tab-separated tables, with no quote character.
 *
 * @author dramage
 */
class TSVTableReader(source : TextReader)
extends TextTableReader(source, '\t', TextTableSerialization.NoQuote);

/**
 * TextWriter for writing to a DelimitedTable. This class is not threadsafe.
 *
 * Usage:
 *
 * <pre>
 *   val table = new TableWriter(sink, ',', '"');
 *   val row1 = table.nextRow();
 *   row1.nextCell().append("text!").finish();
 *   row1.nextCell().append("more").finish();
 *   row1.finish();
 *   val row2 = table.nextRow();
 *   row2.nextCell().append("something").finish();
 *   row2.finish();
 *   table.finish();
 * </pre>
 *
 * @author dramage
 */
class TextTableWriter(val sink : TextWriter, val separator : Int, val quote : Int)
extends TableWriter {

  require(separator >= 0, "Invalid separator");
  require(quote >= 0 || quote == TextTableSerialization.NoQuote, "Invalid quote");

  protected var inTable = true;
  protected var inRow = false;
  protected var inCell = false;
  
  /** Finishes the current cell and then starts the next row. */
  def next() : TableRowWriter = {
    if (!inTable) throw new TableAccessException("Not in table");
    if (inRow) {
      RowWriter.finish();
      sink.append('\n');
    }
    inRow = true;
    RowWriter;
  }

  /** Called when the last cell is written. */
  def finish() {
    if (!inTable) throw new TableAccessException("Not in table");

    if (inRow) {
      RowWriter.finish();
    }
    inTable = false;
  }

  object RowWriter extends TableRowWriter {
    var cellNum = 0;

    /** Finishes the current cell and then starts the next one in row. */
    override def next() = {
      if (!inRow) throw new TableAccessException("Not in row");

      if (inCell) {
        CellWriter.finish();
      }

      if (cellNum > 0) {
        sink.appendCodePoint(separator);
      }
      cellNum += 1;
      inCell = true;
      CellWriter;
    }

    override def finish() = {
      if (!inRow) throw new TableAccessException("Not in row");

      if (inCell) {
        CellWriter.finish();
      }
      sink.append('\n');

      cellNum = 0;
      inRow = false;
    }

    object CellWriter extends TableCellWriter {
      /** We write directly to output if true. */
      protected var cellDirectWrite = false;

      /** We write to cellBuffer if !cellDirectWrite. */
      protected val cellBuffer = new java.lang.StringBuffer();

      override def appendCodePoint(cp : Int) = {
        if (!inCell) throw new TableAccessException("Not in cell");
        
        if (cellDirectWrite) {
          if (cp == quote) {
            sink.appendCodePoint(cp);
            sink.appendCodePoint(cp);
          } else {
            sink.appendCodePoint(cp);
          }
        } else if (cp == separator) {
          if (quote == TextTableSerialization.NoQuote) {
            throw new TableWriteException("Cannot write separator into a cell when quoting is disabled.");
          }
          cellDirectWrite = true;
          sink.appendCodePoint(quote);
          sink.append(cellBuffer.toString);
          cellBuffer.setLength(0);
          sink.appendCodePoint(cp);
        } else if (cp == quote) {
          cellDirectWrite = true;
          sink.appendCodePoint(quote);
          sink.append(cellBuffer.toString);
          cellBuffer.setLength(0);
          sink.appendCodePoint(cp);
          sink.appendCodePoint(cp);
        } else if (cp == '\r' || cp == '\n') {
          if (quote == TextTableSerialization.NoQuote) {
            throw new TableWriteException("Cannot write newline into a cell when quoting is disabled.");
          }
          cellDirectWrite = true;
          sink.appendCodePoint(quote);
          sink.append(cellBuffer.toString);
          cellBuffer.setLength(0);
          sink.appendCodePoint(cp);
        } else {
          cellBuffer.appendCodePoint(cp);
        }
        this;
      }

      override def append(char : Char) =
        appendCodePoint(char);

      override def append(chars : String) = {
        if (!inCell) throw new TableAccessException("Not in cell");

        var i = 0;
        while (i < chars.length) {
          val cp = chars.codePointAt(i);
          appendCodePoint(cp);
          i += Character.charCount(cp);
        }
        this;
      }

      def finish() {
        if (!inCell) throw new TableAccessException("Not in cell");

        if (cellDirectWrite) {
          // if in direct write, then we just need to close quote
          sink.appendCodePoint(quote);
          cellDirectWrite = false;
        } else if (cellBuffer.length > 0) {
          // if not in direct write, then we finished an unescaped cell
          sink.append(cellBuffer.toString);
          cellBuffer.setLength(0);
        }
      }
    }
  }
}

/**
 * For writing CSV formatted tables.
 * 
 * @author dramage
 */
class CSVTableWriter(sink : TextWriter)
extends TextTableWriter(sink, ',', '"');

/**
 * For writing TSV formatted tables.
 *
 * @author dramage
 */
class TSVTableWriter(sink : TextWriter)
extends TextTableWriter(sink, '\t', TextTableSerialization.NoQuote);
