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

import java.io.{BufferedInputStream,BufferedOutputStream}
import java.io.{File,FileInputStream,FileOutputStream}
import java.io.{DataInputStream,DataOutputStream};
import java.io.{InputStreamReader,PrintStream};
import java.io.{ObjectInputStream,ObjectOutputStream};
import java.io.EOFException;

/**
 * Serialization to and from files.  For basic data types that do not have
 * custom file backings, you can import FileSerialization.FromText._ to
 * write using {@link TextSerialization}, FileSerialization.FromData._
 * to write using {@link DataSerialization}, or FileSerialization.FromJava._
 * to write using Java's default serialization mechanism.
 * 
 * @author dramage
 */
object FileSerialization extends SerializationFormat {
  type Input = File;
  type Output = File;

  def readText[T:TextSerialization.Readable](source : Input) =
    read(source)(FromText.fromTextReadable);

  def writeText[T:TextSerialization.Writable](sink : Output, what : T) =
    write(sink, what)(FromText.fromTextWritable);

  def readData[T:DataSerialization.Readable](source : Input) =
    read(source)(FromData.fromDataReadable);

  def writeData[T:DataSerialization.Writable](sink : Output, what : T) =
    write(sink, what)(FromData.fromDataWritable);


  object FromData {
    def fromData[T](implicit rw : DataSerialization.ReadWritable[T]) =
    new ReadWritable[T] {
      override def read(source : File) =
        fromDataReadable[T].read(source);
      override def write(sink : File, v : T) =
        fromDataWritable[T].write(sink, v);
    }

    implicit def fromDataReadable[T](implicit dr : DataSerialization.Readable[T]) =
    new Readable[T] {
      override def read(source : File) = {
        val input = new DataInputStream(new BufferedInputStream(new FileInputStream(source)));
        val rv = try {
          DataSerialization.read[T](input);
        } finally {
          input.close();
        }
        rv;
      }
    }

    implicit def fromDataWritable[T](implicit dw : DataSerialization.Writable[T]) =
    new Writable[T] {
      override def write(sink : File, value : T) = {
        val output = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(sink)));
        try {
          DataSerialization.write[T](output, value);
        } finally {
          output.close();
        }
      }
    }
  }

  object FromText {
    def fromText[T:TextSerialization.ReadWritable] =
    new ReadWritable[T] {
      override def read(source : File) =
        fromTextReadable[T].read(source);
      override def write(sink : File, v : T) =
        fromTextWritable[T].write(sink, v);
    }

    implicit def fromTextReadable[T:TextSerialization.Readable] =
    new Readable[T] {
      val MISSING = -2;

      override def read(source : File) = {
        val input = new InputStreamReader(new BufferedInputStream(new FileInputStream(source)));
        val iterator = new Iterator[Char] {
          var queue : Int = MISSING;

          @inline final def prepare() {
            if (queue == MISSING) queue = input.read();
          }

          override def hasNext = {
            prepare();
            queue >= 0;
          }

          override def next = {
            prepare();
            val rv = queue;
            queue = MISSING;
            if (rv < 0) throw new EOFException();
            rv.toChar;
          }

          override def toString = source.toString;
        }
        try {
          implicitly[TextSerialization.Readable[T]].read(iterator.buffered);
        } finally {
          input.close();
        }
      }
    }

    implicit def fromTextWritable[T:TextSerialization.Writable] =
    new Writable[T] {
      override def write(sink : File, value : T) = {
        val output = new PrintStream(new BufferedOutputStream(new FileOutputStream(sink)));
        try {
          implicitly[TextSerialization.Writable[T]].write(output, value);
        } finally {
          output.close;
        }
      }
    }
  }

  object FromJava {
    implicit def fromJava[T] = new ReadWritable[T] {
      override def read(source : File) = {
        val input = new ObjectInputStream(new BufferedInputStream(new FileInputStream(source)));
        val rv = try {
          input.readObject().asInstanceOf[T];
        } finally {
          input.close();
        }
        rv;
      }

      override def write(sink : File, value : T) = {
        val output = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(sink)));
        try {
          output.writeObject(value);
        } finally {
          output.close();
        }
      }
    }
  }
}
