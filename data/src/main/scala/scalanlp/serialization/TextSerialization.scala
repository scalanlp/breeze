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

import java.io.File;
import scalanlp.io.{TextReader,TextWriter};
import scala.collection.mutable.Builder;

object TextSerialization extends SerializationFormat
with SerializationFormat.PrimitiveTypes with SerializationFormat.CompoundTypes
with StringSerialization {

  type Input = TextReader;
  type Output = TextWriter;

  /** Caches the given value to the given path. */
  def cache[V:Readable:Writable](path : File)(value : =>V) =
    scalanlp.ra.Cell.cache(path)(value)(
      FileSerialization.fromTextReadable[V],
      FileSerialization.fromTextWritable[V]);

  //
  // from StringSerialization
  //

  /** Marshalls the given value as a string. */
  def toString[T:Writable](value: T) : String = {
    val builder = new StringBuilder();
    implicitly[Writable[T]].write(builder,value);
    builder.toString;
  }

  /** Demarshalls a value from the given string. */
  def fromString[T:Readable](str: String) : T = {
    val reader = TextReader.fromString(str);
    val rv = implicitly[Readable[T]].read(reader);
    reader.skipWhitespace;
    if (reader.peek >= 0)
      throw new SerializationException("fromString did not consume whole string: \n"+toString(str));
    rv;
  }

  //
  // from PrimitiveTypes
  //

  implicit val unitReadWritable : ReadWritable[Unit] = new ReadWritable[Unit] {
    override def read(in : Input) = in.expect("()");
    override def write(out : Output, v : Unit) = out.append("()");
  }

  override implicit val intReadWritable : ReadWritable[Int] = new ReadWritable[Int] {
    override def read(in : Input) = java.lang.Integer.parseInt(in.readNumber);
    override def write(out : Output, v : Int) = out.append(v.toString);
  }

  override implicit val byteReadWritable : ReadWritable[Byte] = new this.ReadWritable[Byte] {
    override def read(in : Input) = java.lang.Byte.parseByte(in.readNumber);
    override def write(out : Output, v : Byte) = out.append(v.toString);
  }

  override implicit val longReadWritable : ReadWritable[Long] = new ReadWritable[Long] {
    override def read(in : Input) = java.lang.Long.parseLong(in.readNumber);
    override def write(out : Output, v : Long) = out.append(v.toString);
  }

  override implicit val shortReadWritable : ReadWritable[Short] = new ReadWritable[Short] {
    override def read(in : Input) = java.lang.Short.parseShort(in.readNumber);
    override def write(out : Output, v : Short) = out.append(v.toString);
  }

  override implicit val doubleReadWritable : ReadWritable[Double] = new ReadWritable[Double] {
    // thread-local string-builder for reading floating point numbers.
    val tlsb = new java.lang.ThreadLocal[StringBuilder]() {
      override def initialValue = new StringBuilder(50);
    }
  
    @inline private final def isDigit(c : Int) =
      c >= '0' && c <= '9';
  
    override def read(in : Input) = {
      val switch = in.peek();
      if (switch == '-') {
        in.read(); -read(in);
      } else if (switch == 'n' || switch == 'N') {
        in.expectLower("nan"); Double.NaN;
      } else if (switch == 'i' || switch == 'I') {
        in.expectLower("inf"); Double.PositiveInfinity;
      } else {
        // get thread-local string value
        val sb = tlsb.get;

        // clear current builder
        sb.setLength(0);
        
        // read base
        while (isDigit(in.peek())) { sb.append(in.read().asInstanceOf[Char]); }

        // read decimal
        if (in.peek() == '.') {
          sb.append(in.read().asInstanceOf[Char]);
          while (isDigit(in.peek())) { sb.append(in.read().asInstanceOf[Char]); }
        }

        // read exponent
        if (in.peek() == 'e' || in.peek() == 'E') {
          sb.append(in.read().asInstanceOf[Char]);
          if (in.peek() == '-' || in.peek() == '+') {
            sb.append(in.read().asInstanceOf[Char]);
          }
          while (isDigit(in.peek())) { sb.append(in.read().asInstanceOf[Char]); }
          if (in.peek() == '.') {
            sb.append(in.read().asInstanceOf[Char]);
            while (isDigit(in.peek())) { sb.append(in.read().asInstanceOf[Char]); }
          }
        }

        java.lang.Double.parseDouble(sb.toString)
      }
    }
    
    override def write(out : Output, v : Double) = {
      if (v == Double.PositiveInfinity) out.append("Inf")
      else if (v == Double.NegativeInfinity) out.append("-Inf")
      else out.append(v.toString);
    }
  }

  override implicit val floatReadWritable : ReadWritable[Float] = new ReadWritable[Float] {
    override def read(in : Input) =
      doubleReadWritable.read(in).toFloat;

    override def write(out : Output, v : Float) = {
      if (v == Float.PositiveInfinity) out.append("Inf")
      else if (v == Float.NegativeInfinity) out.append("-Inf")
      else out.append(v.toString);
    }
  }

  override implicit val booleanReadWritable : ReadWritable[Boolean]  = new ReadWritable[Boolean] {
    override def read(in : Input) = {
      val switch = in.peek();
      if (switch == 't' || switch == 'T') {
        in.expectLower("true"); true;
      } else if (switch == 'f' || switch == 'F') {
        in.expectLower("false"); false;
      } else {
        throw new TextSerializationException("Unexpected boolean value");
      }
    }

    override def write(out : Output, v : Boolean) = {
      if (v) out.append("true") else out.append("false");
    }
  }

  override implicit val charReadWritable : ReadWritable[Char] = new ReadWritable[Char] {
    override def read(in : Input) = {
      in.expect('\'');
      val rv = in.read() match {
        case '\\' => in.read() match {
          case 'b'  => '\b';
          case 'f'  => '\f';
          case 'n'  => '\n';
          case 'r'  => '\r';
          case 't'  => '\t';
          case '\'' => '\'';
          case 'u'  => java.lang.Integer.parseInt(in.read(4), 16).toChar;
          case c    => c;
        }
        
        case '\'' =>
          throw new TextSerializationException("Empty character");

        case c => c;
      }
      in.expect('\'');
      rv.toChar;
    }

    override def write(out : Output, c : Char) = {
      out.append('\'');
      out.append(if (c == '\'') "\\'" else escapeChar(c));
      out.append('\'');
    }
  }

  def mkStringReadWritable(quote : Char = '"') : ReadWritable[String] = new ReadWritable[String] {
    override def read(in : Input) = {
      val rv = new java.lang.StringBuilder();
      in.expect(quote);
      while (in.peek() >= 0 && in.peek() != quote) {
        rv.appendCodePoint(
          in.read() match {
            case '\\' => in.read() match {
              case 'b'  => '\b';
              case 'f'  => '\f';
              case 'n'  => '\n';
              case 'r'  => '\r';
              case 't'  => '\t';
              case '"'  => '"';
              case '\\' => '\\';
              case '/'  => '/';
              case 'u'  => java.lang.Integer.parseInt(in.read(4), 16).toChar;
              case c    => throw new TextSerializationException("Unknown escape character "+escapeChar(c.toChar));
            }
            case c : Int => c
          }
        );
      }
      in.expect(quote);
      rv.toString;
    }

    override def write(out : Output, v : String) = {
      out.append(quote);
      out.append(escape(v));
      out.append(quote);
    }
  }

  override implicit val stringReadWritable : ReadWritable[String] =
    mkStringReadWritable('"');

  //
  // from CompoundTypes
  //

  /** Reads a name from the input, consisting of letters, digits, underscore, period, and dollar sign. */
  override def readName(src : Input) : String = {
    val rv = src.readWhile(c => (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' || c == '.' || c == '$');
    if (rv.length == 0)
      throw new TextSerializationException("Expected symbol name");
    rv;
  }

  override def writeName(out : Output, name : String) = {
    out.append(name);
  }

  override protected def readTupleStart(in : Input) = {
    in.expect('(');
    in.skipWhitespace;
  }

  override protected def readTupleGlue(in : Input) = {
    in.skipWhitespace;
    in.expect(',');
    in.skipWhitespace;
  }

  override protected def readTupleEnd(in : Input) = {
    in.skipWhitespace;
    in.expect(')');
  }

  override protected def writeTupleStart(out : Output) =
    out.append('(');

  override protected def writeTupleGlue(out : Output) =
    out.append(',');

  override protected def writeTupleEnd(out : Output) =
    out.append(')');

  override protected def readBuildable[T:Readable,To]
  (in : Input, builder : Builder[T,To]) : To = {
    val name = readName(in);
    in.expect('(');
    in.skipWhitespace;

    while (in.peek() >= 0 && in.peek() != ')') {
      builder += implicitly[Readable[T]].read(in);
      in.skipWhitespace;
      if (in.peek() != ')') {
        in.expect(',');
        in.skipWhitespace;
      }
    }

    in.expect(')');

    builder.result;
  }

  override protected def writeIterable[T:Writable,CC<:Iterable[T]]
  (sink : Output, coll : CC, name : String) {
    if (readName(name) != name)
      throw new TextSerializationException("Not a valid name.");

    sink.append(name);
    sink.append('(');
    val iter = coll.iterator;
    while (iter.hasNext) {
      implicitly[Writable[T]].write(sink,iter.next);
      if (iter.hasNext) sink.append(',');
    }
    sink.append(')');
  }

  //
  // Utility methods
  //

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
      { val hex = c.toHexString.toUpperCase; "\\u"+("0"*(4-hex.length))+hex; }
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
        val hex = cp.toHexString.toUpperCase;
        sb.append("\\u"+("0"*(4-hex.length))+hex);
      }
      else sb.appendCodePoint(cp);

      i += Character.charCount(cp);
    }

    sb.toString;
  }
}

/**
 * Exception thrown during text serialization or deserialization.
 *
 * @author dramage
 */
class TextSerializationException(msg : String) extends RuntimeException(msg);
