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
package breeze;
package io;

/**
 * Reader for consuming unicode code points from a stream.  See implicit
 * constructors in companion object.  This class is not threadsafe.
 *
 * @author dramage
 */
trait TextReader { self =>

  /** Returns the next unicode code point to be read, or -1 if the end of input. */
  def read() : Int;

  /** Peeks at the next unicode code point without consuming it, or -1 if the end of input. */
  def peek() : Int;

  /** Closes the input. */
  def close();

  /** Throws a TextReaderException at the current line and column. */
  def die(msg : String) =
    throw new TextReaderException(msg, lineNumber, columnNumber);

  /**
   * Peeks at the character at offset n.  With n == 0 is the same as peek().
   * This method might not be implemented for all n>0 in all TextReader
   * implementations, in which case it will throw an IllegalArgumentException.
   */
  def peek(n : Int) : Int;

  /** Returns the current line number in the reader (1-based). */
  def lineNumber : Int;

  /** Returns the current column number in the reader (1-based). */
  def columnNumber : Int;

  /** Returns true if there are no more characters left to consume. */
  def isEmpty : Boolean =
    peek() == -1;

  private val builder = new java.lang.StringBuilder();

  /** Reads up to numChars characters from the input, or fewer if at the end of input. */
  def read(numChars : Int) : String = {
    builder.setLength(0);
    var c = peek();
    var i = 0;
    while (i < numChars && c >= 0) {
      builder.appendCodePoint(read());
      c = peek();
      i += 1;
    }
    builder.toString;
  }

  /** Reads the next line from the input. Does not return the newline, which is left in the stream.  Returns null if there are no more lines. */
  def readLine() : String = {
    builder.setLength(0);
    var c = peek();

    if (c < 0) {
      return null;
    }

    while (c >= 0 && c != '\r' && c != '\n') {
      builder.appendCodePoint(read());
      c = peek();
    }

    builder.toString;
  }

  /** Reads and returns a newline character at the current position.  Throws an exception if a newline does not follow. */
  def readNewline() : String = {
    val c = read();
    if (c == '\r' && peek() == '\n') {
      read();
      "\r\n";
    } else if (c == '\r') {
      "\r"
    } else if (c == '\n') {
      "\n"
    } else {
      die("Expected newline but got: '"+TextReader.escapeChar(c.toChar)+"'");
    }
  }

  /** Consumes the characters from input while available and while the predicate matches. Returns null if there are no more characters. */
  def readWhile(fn : Int => Boolean) : String = {
    builder.setLength(0);
    var c = peek();

    if (c < 0) {
      return null;
    }

    while (c >= 0 && fn(c)) {
      builder.appendCodePoint(read());
      c = peek();
    }
    builder.toString;
  }

  /** Returns the rest of the characters in the input as a String. */
  def readRemaining() : String = {
    builder.setLength(0);
    var c = read();
    while(c >= 0) {
      builder.appendCodePoint(c);
      c = read();
    }
    builder.toString;
  }

  /** Skips characters while the given predicate is true. */
  def skipWhile(fn : Int => Boolean) : Unit = {
    var c = peek();
    while (c >= 0 && fn(c)) {
      read();
      c = peek();
    }
  }

  /** Skips whitespace characters. */
  def skipWhitespace() : Unit =
    skipWhile((cp : Int) => { val ch = cp.toChar; ch == cp && ch.isWhitespace; });

  /** Expect the given literal, throwing an exception on mismatch. */
  def expect(literal : Char) : Unit = {
    val got = read();
    if (got != literal) {
      die("Got: "+TextReader.escapeChar(got.toChar)+" != "+TextReader.escapeChar(literal));
    }
  }

  /** Expect the given literal (after lowercasing). */
  def expectLower(literal : Char) : Unit = {
    val got = Character.toLowerCase(read());
    if (got != literal) {
      die("Got: "+TextReader.escapeChar(got.toChar)+" != "+TextReader.escapeChar(literal));
    }
  }

  /** Expect the given literal, throwing an exception on mismatch. */
  def expect(literal : String) : Unit = {
    val got = read(literal.length);
    if (got != literal) {
      die("Got: "+TextReader.escape(got)+" != "+TextReader.escape(literal));
    }
  }

  /** Expect the given literal (after lowercasing). */
  def expectLower(literal : String) : Unit = {
    val got = read(literal.length).toLowerCase;
    if (got != literal) {
      die("Got: "+TextReader.escape(got)+" != "+TextReader.escape(literal));
    }
  }


  /** Read a sequence of character consisting of the digits '0' to '9'.  Returns null if there are no more characters. */
  def readNumber() : String = {
    builder.setLength(0);
    var c = peek();

    if (c < 0) {
      return null;
    }

    if (c == '-') {
      builder.appendCodePoint(read());
      c = peek();
    }

    while (c >= '0' && c <= '9') {
      builder.appendCodePoint(read());
      c = peek();
    }

    builder.toString;
  }

  //
  // Collection-style operations
  //

  /** Concatenates two TextReaders together. */
  def ++(next : TextReader) = {
    val curr = this;
    new TextReader {
      override def lineNumber = -1;
      override def columnNumber = -1;

      override def read() =
        if (curr.peek() >= 0) curr.read() else next.read();

      override def peek() =
        if (curr.peek() >= 0) curr.peek() else next.peek();

      override def peek(n : Int) =
        if (n == 0) peek() else throw new IllegalArgumentException("Can only peek(0)");

      override def close() = {
        curr.close();
        next.close();
      }
    }
  }
}

object TextReader {
  implicit def fromString(string : String) =
    new StringReader(string);

  implicit def fromInputStream(stream : java.io.InputStream) =
    new InputStreamReader(stream);

  implicit def fromReader(reader : java.io.Reader) = {
    if (reader.isInstanceOf[java.io.BufferedReader])
      new ReaderReader(reader.asInstanceOf[java.io.BufferedReader])
    else
      new ReaderReader(new java.io.BufferedReader(reader));
  }

  implicit def fromFile(file : java.io.File) =
    new FileReader(file);

  /**
   * A TextReader that reads from a String.
   * 
   * @author dramage
   */
  class StringReader(val string : String)
  extends ReaderReader(
    new java.io.BufferedReader(
      new java.io.StringReader(string)));

  /**
   * A TextReader that reads from an InputStream.
   *
   * @author dramage
   */
  class InputStreamReader(val stream : java.io.InputStream)
  extends ReaderReader(
    new java.io.BufferedReader(
      new java.io.InputStreamReader(stream)));

  /**
   * A TextReader that reads from a File.
   *
   * @author dramage
   */
  class FileReader(val file : java.io.File)
  extends InputStreamReader(FileStreams.input(file));

  /**
   * A TextReader that reads from a Java Reader.
   *
   * @author dramage
   */
  class ReaderReader(val reader : java.io.BufferedReader) extends TextReader {
    var lineNo : Int = 1;
    var colNo : Int = 0;

    override def lineNumber = lineNo;
    override def columnNumber = colNo;

    var next = reader.read;

    override def read() = {
      val rv = next;
      next = reader.read;

      if (rv == '\n') {
        lineNo += 1;
        colNo = 0;
      } else {
        colNo += 1;
      }

      rv;
    }

    override def peek() =
      next;

    override def peek(n : Int) = {
      if (n == 0) {
        next;
      } else {
        reader.mark(n);
        reader.skip(n-1);
        val rv = reader.read;
        reader.reset();
        rv;
      }
    }

    override def close() =
      reader.close();
  }

  // utility methods
  /** Escapes the given string.  Unicode friendly, but only for code points that fit inside a Char. */
  def escapeChar(c : Char) : String = c match {
    case '"'  => "\\\"";
    case '\\' => "\\\\";
    case '\b' => "\\b";
    case '\f' => "\\f";
    case '\n' => "\\n";
    case '\r' => "\\r";
    case '\t' => "\\t";
    case c if ((c >= '\u0000' && c <= '\u001F') || (c >= '\u007F' && c <= '\u009F') || (c >= '\u2000' && c <= '\u20FF')) =>
      { val hex = c.toInt.toHexString.toUpperCase; "\\u"+("0"*(4-hex.length))+hex; }
    case c => c.toString;
  }



  /** Escapes the given string.  Unicode friendly. */
  def escape(str : String) : String = {
    val sb = new java.lang.StringBuilder;

    var i = 0;
    while (i < str.length) {
      val cp = str.codePointAt(i);

      if (cp == '"') sb.append("\\\"");
      else if (cp == '\\') sb.append("\\\\");
      else if (cp == '\b') sb.append("\\b");
      else if (cp == '\f') sb.append("\\f");
      else if (cp == '\n') sb.append("\\n");
      else if (cp == '\r') sb.append("\\r");
      else if (cp == '\t') sb.append("\\t");
      else if ((cp >= '\u0000' && cp <= '\u001F') || (cp >= '\u007F' && cp <= '\u009F') || (cp >= '\u2000' && cp <= '\u20FF')) {
        val hex = cp.toInt.toHexString.toUpperCase;
        sb.append("\\u"+("0"*(4-hex.length))+hex);
      }
      else sb.appendCodePoint(cp);

      i += Character.charCount(cp);
    }

    sb.toString;
  }
}

/**
 * Thrown when encountering a problem while reading from a TextReader.
 *
 * @author dramage
 */
class TextReaderException(msg : String, cause : Throwable, val lineNo : Int, val colNo : Int)
extends RuntimeException(msg + " at line " + lineNo + " column " + colNo, cause) {
  def this(msg : String, lineNo : Int, colNo : Int) =
    this(msg, null, lineNo, colNo);
}
