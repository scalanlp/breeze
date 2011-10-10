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
package scalanlp.serialization

import java.io.{InputStream,OutputStream};
import java.io.{BufferedInputStream,BufferedOutputStream};
import java.io.{File,FileInputStream,FileOutputStream};
import java.io.{DataInputStream,DataOutputStream};
import java.io.{InputStreamReader,PrintStream};
import java.io.{ObjectInputStream,ObjectOutputStream};
import java.io.EOFException;
import java.util.zip.{GZIPInputStream, GZIPOutputStream};

import scalanlp.io.FileStreams;

/**
 * Serialization to and from files.  Classes that wish to provide a
 * File backing may do so directly, either by endowing companion objects
 * with an implicit converstion to FileSerialization.ReadWritable or by
 * mixing in FileSerializationFromText[X] or FileSerializationFromData[X] if
 * the corresponding text or data format is available.  Companion objects
 * may alternative mix in FileSerializationFromJava[X] to fall back to
 * default Java serialization, if possible.
 * 
 * @author dramage
 */
object FileSerialization extends SerializationFormat
with LowPriorityFileSerializationImplicits {
  type Input = File;
  type Output = File;

  def readText[T:TextSerialization.Readable](source : Input) =
    read(source)(fromTextReadable);

  def writeText[T:TextSerialization.Writable](sink : Output, what : T) =
    write(sink, what)(fromTextWritable);

  def readData[T:DataSerialization.Readable](source : Input) =
    read(source)(fromDataReadable);

  def writeData[T:DataSerialization.Writable](sink : Output, what : T) =
    write(sink, what)(fromDataWritable);

  def readJava[T](source : Input) =
    read(source)(GenericFileSerializationFromJava.fromJava[T]);

  def writeJava[T](sink : Output, what : T) =
    write(sink, what)(GenericFileSerializationFromJava.fromJava[T]);
}

/** Mix-in trait used by FileSerialization to prioritize Text before Data.  @author dramage */
trait LowPriorityFileSerializationImplicits
extends GenericFileSerializationFromData {
  implicit def fromTextReadable[T:TextSerialization.Readable] =
    GenericFileSerializationFromText.fromTextReadable;

  implicit def fromTextWritable[T:TextSerialization.Writable] =
    GenericFileSerializationFromText.fromTextWritable;
}


/**
 * Mix-in trait for companion objects to leverage TextSerialization as the
 * default format for FileSerialization for their associated type.
 *
 * @author dramage
 */
trait FileSerializationFromText[T] {
  implicit def fileReadableFromTextReadable
  (implicit textReadable : TextSerialization.Readable[T]) =
    GenericFileSerializationFromText.fromTextReadable[T];
}

/**
 * Implicit conversion from any TextSerialization readable and writable to
 * FileSerialization readable and writable.
 *
 * @author dramage
 */
trait GenericFileSerializationFromText {

  implicit def fromTextReadable[T](implicit tr :TextSerialization.Readable[T])
  : FileSerialization.Readable[T] =
  new FileSerialization.Readable[T] {
    val MISSING = -2;

    override def read(source : File) = {
      val input = new InputStreamReader(FileStreams.input(source));
      try {
        tr.read(input);
      } finally {
        if (!tr.streaming) {
          input.close();
        }
      }
    }
  }

  implicit def fromTextWritable[T:TextSerialization.Writable] : FileSerialization.Writable[T]  =
  new FileSerialization.Writable[T] {
    override def write(sink : File, value : T) = {
      val output = new PrintStream(FileStreams.output(sink));
      try {
        implicitly[TextSerialization.Writable[T]].write(output, value);
      } finally {
        output.close;
      }
    }
  }
}

object GenericFileSerializationFromText
extends GenericFileSerializationFromText;


/**
 * Mix-in trait for companion objects to leverage DataSerialization as the
 * default format for FileSerialization for their associated type.
 *
 * @author dramage
 */
trait FileSerializationFromData[T] {
  implicit def fileReadableFromDataReadable
  (implicit readable : DataSerialization.Readable[T]) =
    GenericFileSerializationFromData.fromDataReadable[T];
}

/**
 * Implicit conversion from any DataSerialization readable and writable to
 * FileSerialization readable and writable.
 *
 * @author dramage
 */
trait GenericFileSerializationFromData {

  implicit def fromDataReadable[T](implicit dr : DataSerialization.Readable[T]) : FileSerialization.Readable[T]  =
  new FileSerialization.Readable[T] {
    override def read(source : File) = {
      val input = new DataInputStream(FileStreams.input(source));
      val rv = try {
        DataSerialization.read[T](input);
      } finally {
        if (!dr.streaming) {
          input.close();
        }
      }
      rv;
    }
  }

  implicit def fromDataWritable[T](implicit dw : DataSerialization.Writable[T]) : FileSerialization.Writable[T]  =
  new FileSerialization.Writable[T] {
    override def write(sink : File, value : T) = {
      val output = new DataOutputStream(FileStreams.output(sink));
      try {
        DataSerialization.write[T](output, value);
      } finally {
        output.close();
      }
    }
  }
}

object GenericFileSerializationFromData
extends GenericFileSerializationFromData;


/**
 * Mix-in trait for companion objects to leverage built-in Java serialization
 * as the default format for FileSerialization for their associated type.
 *
 * @author dramage
 */
trait FileSerializationFromJava[T] {
  implicit def fromJava = GenericFileSerializationFromJava.fromJava[T];
}

/**
 * This trait provides low-priority conversions for FileSerialization to
 * any object that is serialization using Java's serialization mechanism.
 * Precedence is determined by its mix-in order in FileSerialization.
 *
 * @author dramage
 */
object GenericFileSerializationFromJava {
  implicit def fromJava[T] : FileSerialization.ReadWritable[T] =
  new FileSerialization.ReadWritable[T] {
    override def read(source : File) = {
      val input = new ObjectInputStream(FileStreams.input(source));
      val rv = try {
        input.readObject().asInstanceOf[T];
      } finally {
        input.close();
      }
      rv;
    }

    override def write(sink : File, value : T) = {
      val output = new ObjectOutputStream(FileStreams.output(sink));
      try {
        output.writeObject(value);
      } finally {
        output.close();
      }
    }
  }
}
