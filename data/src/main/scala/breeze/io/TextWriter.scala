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
 * A simple append-based text writing interface used by TextSerialization
 * as its Output type.  See implicit constructors in companion object.
 *
 * @author dramage
 */
trait TextWriter {
  /** Appends the given character and returns this. */
  def append(char : Char) : TextWriter;

  /** Appends the given string and returns this. */
  def append(string : String) : TextWriter;

  /** Appends the given unicode code point and returns this. */
  def appendCodePoint(codePoint: Int) : TextWriter;

  /** Closes the output. */
  def close();
}

object TextWriter {
  implicit def fromStringBuilder(sb : StringBuilder) =
    new StringBuilderWriter(sb);

  implicit def fromPrintStream(ps : java.io.PrintStream) =
    new PrintStreamWriter(ps);

  implicit def fromFile(file : java.io.File) =
    new FileWriter(file);

  /**
   * A TextWriter that appends to a StringBuilder.
   *
   * @author dramage
   */
  class StringBuilderWriter(val sb : StringBuilder) extends TextWriter {
    override def append(char : Char) =
      { sb.append(char); this; }
    override def append(string : String) =
      { sb.append(string); this; }
    override def appendCodePoint(cp : Int) = {
      if (cp == cp.asInstanceOf[Char])
        sb.append(cp.asInstanceOf[Char])
      else
        sb.append(new String(Character.toChars(cp)));
      this;
    }
    override def close() = { /* do nothing */ }
  }

  /**
   * A TextWriter that writes to a Writer.
   *
   * @author dramage
   */
  class WriterWriter(val osw : java.io.Writer) extends TextWriter {
    override def append(char : Char) =
      { osw.write(char); this; }
    override def append(string : String) =
      { osw.write(string); this; }
    override def appendCodePoint(cp : Int) =
      { osw.write(cp); this; }
    override def close =
      { osw.close; }
  }

  /**
   * A TextWriter that writes to a PrintStream.
   *
   * @author dramage
   */
  class PrintStreamWriter(val ps : java.io.PrintStream) extends TextWriter {
    override def append(char : Char) =
      { ps.append(char); this; }
    override def append(string : String) =
      { ps.append(string); this; }
    override def appendCodePoint(cp : Int) = {
      if (cp == cp.asInstanceOf[Char])
        ps.append(cp.asInstanceOf[Char])
      else
        ps.append(new String(Character.toChars(cp)));
      this;
    }
    override def close() =
      ps.close();
  }

  /**
   * A TextWriter that writes to a File.
   * 
   * @author dramage
   */
  class FileWriter(val file : java.io.File)
  extends WriterWriter(
    new java.io.BufferedWriter(
      new java.io.OutputStreamWriter(
        FileStreams.output(file))));
}

