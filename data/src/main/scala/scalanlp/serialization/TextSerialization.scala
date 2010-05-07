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

import scala.collection.mutable.Builder;

object TextSerialization extends SerializationFormat
with SerializationFormat.PrimitiveTypes with SerializationFormat.CompoundTypes
with ByteSerialization with StringSerialization {

  type Input = BufferedIterator[Char];
  type Output = TextWriter;

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
  def fromString[T:Readable](str: String) : T =
    implicitly[Readable[T]].read(str.iterator.buffered);

  //
  // from ByteSerialization
  //

  override def toBytes[T:Writable](x: T) =
    toString(x).getBytes("utf8");

  override def fromBytes[T:Readable](bytes: Array[Byte]) : T =
    fromString[T](new String(bytes,"utf8"));

  //
  // from PrimitiveTypes
  //

  override implicit val intReadWritable : ReadWritable[Int] = new ReadWritable[Int] {
    override def read(in : Input) = consumeWhile(in, c => c.isDigit || c=='-').toInt;
    override def write(out : Output, v : Int) = out.append(v.toString);
  }

  override implicit val byteReadWritable : ReadWritable[Byte] = new this.ReadWritable[Byte] {
    override def read(in : Input) = consumeWhile(in, c => c.isDigit || c=='-').toByte;
    override def write(out : Output, v : Byte) = out.append(v.toString);
  }

  override implicit val longReadWritable : ReadWritable[Long] = new ReadWritable[Long] {
    override def read(in : Input) = consumeWhile(in, c => c.isDigit || c=='-').toLong;
    override def write(out : Output, v : Long) = out.append(v.toString);
  }

  override implicit val shortReadWritable : ReadWritable[Short] = new ReadWritable[Short] {
    override def read(in : Input) = consumeWhile(in, c => c.isDigit || c=='-').toShort;
    override def write(out : Output, v : Short) = out.append(v.toString);
  }
  
  override implicit val doubleReadWritable : ReadWritable[Double] = new ReadWritable[Double] {
    override def read(in : Input) = in.head.toLower match {
      case '-' => { in.next; -read(in); }
      case 'n' => { expect(in, "nan", true); Double.NaN; }
      case 'i' => { expect(in, "inf", true); Double.PositiveInfinity; }
      case _ => {
        consumeWhile(in, c => c.isDigit || c.toLower == 'e' || c == '+' || c == '-' || c == '.').toDouble;
      }
    }
    
    override def write(out : Output, v : Double) = {
      if (v == Double.PositiveInfinity) out.append("Inf")
      else if (v == Double.NegativeInfinity) out.append("-Inf")
      else out.append(v.toString);
    }
  }

  override implicit val floatReadWritable : ReadWritable[Float] = new ReadWritable[Float] {
    override def read(in : Input) = in.head.toLower match {
      case '-' => { in.next; -read(in); }
      case 'n' => { expect(in, "nan", true); Float.NaN; }
      case 'i' => { expect(in, "inf", true); Float.PositiveInfinity; }
      case _ => {
        consumeWhile(in, c => c.isDigit || c.toLower == 'e' || c == '+' || c == '-' || c == '.').toFloat;
      }
    }

    override def write(out : Output, v : Float) = {
      if (v == Float.PositiveInfinity) out.append("Inf")
      else if (v == Float.NegativeInfinity) out.append("-Inf")
      else out.append(v.toString);
    }
  }

  override implicit val booleanReadWritable : ReadWritable[Boolean]  = new ReadWritable[Boolean] {
    override def read(in : Input) = {
      in.head match {
        case 't' => { expect(in, "true", true); true; }
        case 'f' => { expect(in, "false", true); false; }
        case _ => throw new TextSerializationException("Unexpected boolean value");
      }
    }

    override def write(out : Output, v : Boolean) = {
      if (v) out.append("true") else out.append("false");
    }
  }

  override implicit val charReadWritable : ReadWritable[Char] = new ReadWritable[Char] {
    override def read(in : Input) = {
      expect(in, '\'', false);
      val rv = in.next match {
        case '\\' => in.next match {
          case 'b'  => '\b';
          case 'f'  => '\f';
          case 'n'  => '\n';
          case 'r'  => '\r';
          case 't'  => '\t';
          case '\'' => '\'';
          case 'u'  => java.lang.Integer.parseInt(consume(in, 4), 16).toChar;
          case c    => c;
        }
        
        case '\'' =>
          throw new TextSerializationException("Empty character");

        case c => c;
      }
      expect(in, '\'', false);
      rv;
    }

    override def write(out : Output, c : Char) = {
      out.append('\'');
      out.append(if (c == '\'') "\\'" else escapeChar(c));
      out.append('\'');
    }
  }
  
  override implicit val stringReadWritable : ReadWritable[String] = new ReadWritable[String] {
    override def read(in : Input) = {
      expect(in, '"', false);
      val rv = new StringBuilder();
      while (in.head != '"') {
        rv += (in.next match {
          case '\\' => in.next match {
            case 'b'  => '\b';
            case 'f'  => '\f';
            case 'n'  => '\n';
            case 'r'  => '\r';
            case 't'  => '\t';
            case '"'  => '"';
            case '\\' => '\\';
            case '/'  => '/';
            case 'u'  => java.lang.Integer.parseInt(consume(in, 4), 16).toChar;
            case c    => throw new TextSerializationException("Unknown escape character "+escapeChar(c));
          }
          case c : Char => c
        });
      }
      expect(in, '"', false);
      rv.toString;
    }

    override def write(out : Output, v : String) = {
      out.append('"');
      out.append(escape(v));
      out.append('"');
    }
  }

  //
  // from CompoundTypes
  //

  override protected def readTupleStart(in : Input) = {
    expect(in,'(',false);
    skipWhitespace(in);
  }

  override protected def readTupleGlue(in : Input) = {
    skipWhitespace(in);
    expect(in,',',false);
    skipWhitespace(in);
  }

  override protected def readTupleEnd(in : Input) = {
    skipWhitespace(in);
    expect(in,')',false);
  }

  override protected def writeTupleStart(out : Output) =
    out.append('(');

  override protected def writeTupleGlue(out : Output) =
    out.append(',');

  override protected def writeTupleEnd(out : Output) =
    out.append(')');

  override protected def readBuildable[T:Readable,To]
  (src : Input, builder : Builder[T,To]) : To = {
    val name = readName(src);
    expect(src, '(', false);
    skipWhitespace(src);

    while (src.head != ')') {
      builder += implicitly[Readable[T]].read(src);
      skipWhitespace(src);
      if (src.head != ')') {
        expect(src,',',false);
        skipWhitespace(src);
      }
    }

    expect(src, ')', false);

    builder.result;
  }

  override protected def writeIterable[T:Writable,CC<:Iterable[T]]
  (sink : Output, coll : CC, name : String) {
    if (readName(name.iterator.buffered) != name)
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

  def readName(src : Input) : String = {
    val rv = consumeWhile(src, c => c.isLetterOrDigit || c == '_' || c == '.' || c == '$');
    if (rv.length == 0)
      throw new TextSerializationException("Expected symbol name");
    rv;
  }

  def escapeChar(c : Char) : String = c match {
    case '"'  => "\\\"";
    case '\\' => "\\\\";
    case '/'  => "\\/";
    case '\b' => "\\b";
    case '\f' => "\\f";
    case '\n' => "\\n";
    case '\r' => "\\r";
    case '\t' => "\\t";
    case c if ((c >= '\u0000' && c <= '\u001F') || (c >= '\u007F' && c <= '\u009F') || (c >= '\u2000' && c <= '\u20FF')) =>
      { val hex = c.toHexString.toUpperCase; "\\u"+("0"*(4-hex.length))+hex; }
    case c => ""+c;
  }

  protected def escape(str : String) : String =
    str.flatMap(escapeChar)

  /** Throws an exception if the input does not start with the given "expected" string. */
  def expect(in : Input, expected : String, caseFold : Boolean) : Unit = {
    var got = consume(in, expected.length).mkString;
    if (caseFold) got = got.toLowerCase;
    if (got != expected)
      throw new TextSerializationException("Got: "+escape(got)+" != "+escape(expected));
  }

  /** Throws an exception if the input does not start with the given "expected" char. */
  def expect(in : Input, expected : Char, caseFold : Boolean) : Unit = {
    var got = in.next;
    if (caseFold) got = got.toLower;
    if (got != expected)
      throw new TextSerializationException("Got: "+escapeChar(got)+" != "+escapeChar(expected));
  }

  /** Consumes numChars characters from input, or fewer if at the end of input. */
  def consume(in : Input, numChars : Int) : String = {
    val rv = new StringBuilder();
    var i = 0;
    while (i < numChars && in.hasNext) {
      rv += in.next;
      i += 1;
    }
    rv.toString;
  }

  /** Consumes the characters from input while available and while the predicate matches. */
  def consumeWhile(in : Input, p : Char => Boolean) : String = {
    val rv = new StringBuilder();
    while (in.hasNext && p(in.head)) {
      rv += in.next;
    }
    rv.toString;
  }

  /** Skips characters while the given predicate is true. */
  def skipWhile(in : Input, p : Char => Boolean) : Unit =
    while (in.hasNext && p(in.head)) in.next;

  def skipWhitespace(in : Input) : Unit =
    skipWhile(in, _.isWhitespace);
}

/**
 * Exception thrown during text serialization or deserialization.
 *
 * @author dramage
 */
class TextSerializationException(msg : String) extends RuntimeException(msg);


/**
 * A simple append-based text writing interface used by TextSerialization
 * as its Output type.  See implicit conversions in companion object.
 *
 * @author dramage
 */
trait TextWriter {
  def append(char : Char);
  def append(string : String);
}

object TextWriter {
  implicit def fromStringBuilder(sb : StringBuilder) : TextWriter = {
    new TextWriter() {
      override def append(char : Char) = sb.append(char);
      override def append(string : String) = sb.append(string);
    }
  }

  implicit def fromPrintStream(ps : java.io.PrintStream) : TextWriter = {
    new TextWriter() {
      override def append(char : Char) = ps.append(char);
      override def append(string : String) = ps.append(string);
    }
  }
}
