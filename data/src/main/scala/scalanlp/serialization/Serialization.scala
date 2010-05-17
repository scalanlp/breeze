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

import scala.collection.MapLike
import scala.collection.generic.{GenericCompanion, MapFactory}

import scala.collection.mutable.{ArrayBuilder,Builder};

import scalanlp.util.Index


class SerFile(path : String) extends File(path);

class TxtFile(path : String) extends File(path);

class CsvFile(path : String) extends File(path);

/**
 * A base trait for brokers of serialization. See DataSerialization for a good example.
 *
 * Implementors should provide member types Input and Output which are the places the data
 * can be stored. Readable and Writable implicit instances should be created for types
 * that can be serialized in this format. If you also inherit from CompoundTypes,
 * you get a lot of functionality for almost free.
 *
 * @author dlwh
 * @author dramage
 */
trait SerializationFormat {
  /** The place to read data from. */
  type Input;

  /** The place to write data to. */
  type Output;

  /** Inner trait for reading from Input. */
  trait Readable[T] {
    def read(source: Input): T
  }

  /** Inner trait for writing to Output. */
  trait Writable[T] {
    def write(sink: Output, what: T): Unit
  }

  /** A convenience wrapper for Readable and Writable. */
  trait ReadWritable[T] extends Readable[T] with Writable[T];

  def getReadWritable[T:ReadWritable] : ReadWritable[T] =
    implicitly[ReadWritable[T]];

  /** Sugar for implicitly[Readable[T]].read(source); */
  def read[T:Readable](source: Input): T =
    implicitly[Readable[T]].read(source);

  /** Sugar for implicitly[Writable[T]].write(sink,what); */
  def write[T:Writable](sink: Output, what:T):Unit =
    implicitly[Writable[T]].write(sink, what);
}

object SerializationFormat {
  /**
   * Supports reading and writing standard primitive types and String.
   * 
   * @author dramage
   */
  trait PrimitiveTypes { this : SerializationFormat =>
    implicit val intReadWritable : ReadWritable[Int];
    implicit val byteReadWritable : ReadWritable[Byte];
    implicit val longReadWritable : ReadWritable[Long];
    implicit val shortReadWritable: ReadWritable[Short];
    implicit val doubleReadWritable : ReadWritable[Double];
    implicit val floatReadWritable : ReadWritable[Float];
    implicit val charReadWritable : ReadWritable[Char];
    implicit val booleanReadWritable : ReadWritable[Boolean];
    implicit val stringReadWritable : ReadWritable[String];
  }

  /**
   * Supports reading and writing tuples, collections, maps, arrays, etc.,
   * if their underlying key and value types are readable and writable.
   *
   * @author dlwh
   * @author dramage
   */
  trait CompoundTypes extends SerializationFormat { this: SerializationFormat =>
    /** Reads elements of type T into the given buildable. Inverse of writeIterable. */
    protected def readBuildable[T:Readable,To]
    (src: Input, builder : Builder[T,To]) : To;

    /** Writes elements of the given collection. Inverse of readBuildable. */
    protected def writeIterable[T:Writable,CC<:Iterable[T]]
    (sink: Output, coll : CC, name : String);

    protected def readTupleStart(in : Input) =
      { /* do nothing */ }

    protected def readTupleGlue(in : Input) =
      { /* do nothing */ }

    protected def readTupleEnd(in : Input) =
      { /* do nothing */ }

    protected def writeTupleStart(out : Output) =
      { /* do nothing */ }

    protected def writeTupleGlue(out : Output) =
      { /* do nothing */ }

    protected def writeTupleEnd(out : Output) =
      { /* do nothing */ }

    /** Standard collection types. */
    protected def collectionFromElements
    [T:ReadWritable,CC[T]<:Iterable[T]]
    (c: GenericCompanion[CC], name : String)
    = new ReadWritable[CC[T]] {
      def read(source : Input) =
        readBuildable[T,CC[T]](source, c.newBuilder[T]);

      def write(sink : Output, coll : CC[T]) =
        writeIterable[T,CC[T]](sink, coll, name);
    }

    /** Map collection types. */
    protected def collectionFromElements
    [K:ReadWritable,V:ReadWritable,CC[K,V]<:Map[K,V] with MapLike[K,V,CC[K,V]]]
    (c: MapFactory[CC], name : String)
    = new ReadWritable[CC[K,V]] {
      def read(source : Input) =
        readBuildable[(K,V),CC[K,V]](source, c.newBuilder[K,V]);

      def write(sink : Output, coll : CC[K,V]) =
        writeIterable[(K,V),CC[K,V]](sink, coll, name);
    }

    implicit def tuple2ReadWritable[T1,T2]
    (implicit t1H: ReadWritable[T1], t2H: ReadWritable[T2])
    = new ReadWritable[(T1,T2)] {
      def read(in: Input) = {
        readTupleStart(in);
        val t1 = t1H.read(in);
        readTupleGlue(in);
        val t2 = t2H.read(in);
        readTupleEnd(in);
        (t1,t2)
      }
      def write(out: Output, t: (T1,T2)) {
        writeTupleStart(out);
        t1H.write(out, t._1);
        writeTupleGlue(out);
        t2H.write(out, t._2);
        writeTupleEnd(out);
      }
    }

    implicit def tuple3ReadWritable[T1,T2,T3]
    (implicit t1H: ReadWritable[T1], t2H: ReadWritable[T2], t3H: ReadWritable[T3])
    = new ReadWritable[(T1,T2,T3)] {
      def read(in: Input) = {
        readTupleStart(in);
        val t1 = t1H.read(in);
        readTupleGlue(in);
        val t2 = t2H.read(in);
        readTupleGlue(in);
        val t3 = t3H.read(in);
        readTupleEnd(in);
        (t1,t2,t3);
      }
      def write(out: Output, t: (T1,T2,T3)) {
        writeTupleStart(out);
        t1H.write(out, t._1);
        writeTupleGlue(out);
        t2H.write(out, t._2);
        writeTupleGlue(out);
        t3H.write(out, t._3);
        writeTupleEnd(out);
      }
    }

    implicit def tuple4ReadWritable[T1,T2,T3,T4]
    (implicit t1H: ReadWritable[T1], t2H: ReadWritable[T2],
     t3H: ReadWritable[T3], t4H: ReadWritable[T4])
    = new ReadWritable[(T1,T2,T3,T4)] {
      def read(in: Input) = {
        readTupleStart(in);
        val t1 = t1H.read(in);
        readTupleGlue(in);
        val t2 = t2H.read(in);
        readTupleGlue(in);
        val t3 = t3H.read(in);
        readTupleGlue(in);
        val t4 = t4H.read(in);
        readTupleEnd(in);
        (t1,t2,t3,t4)
      }
      def write(out: Output, t: (T1,T2,T3,T4)) {
        writeTupleStart(out);
        t1H.write(out, t._1);
        writeTupleGlue(out);
        t2H.write(out, t._2);
        writeTupleGlue(out);
        t3H.write(out, t._3);
        writeTupleGlue(out);
        t4H.write(out, t._4);
        writeTupleEnd(out);
      }
    }

    implicit def arrayReadWritable[T]
    (implicit tH: ReadWritable[T], man: ClassManifest[T])
    = new ReadWritable[Array[T]] {
      def read(source: Input) =
        readBuildable[T,Array[T]](source, ArrayBuilder.make[T]);

      def write(sink: Output, value: Array[T]) =
        writeIterable[T,Seq[T]](sink, value, "Array");
    }

    implicit def listReadWritable[T](implicit tH: ReadWritable[T]) =
      collectionFromElements[T,List](List,"List");

    implicit def seqReadWritable[T](implicit tH: ReadWritable[T]) =
      collectionFromElements[T,Seq](Seq,"Seq");

    implicit def indexedSeqReadWritable[T](implicit tH: ReadWritable[T]) =
      collectionFromElements[T,IndexedSeq](IndexedSeq,"IndexedSeq");

    implicit def setReadWritable[T](implicit tH: ReadWritable[T]) =
      collectionFromElements[T,Set](Set,"Set");

    implicit def mapReadWritable[K:ReadWritable,V:ReadWritable] =
      collectionFromElements[K,V,Map](Map,"Map");

    implicit def indexReadWritable[T:ReadWritable]
    = new ReadWritable[Index[T]] {
      def read(source: Input): Index[T] =
        Index(seqReadWritable[T].read(source));

      def write(sink: Output, value: Index[T]) =
        writeIterable[T,Iterable[T]](sink, value, "Index");
    }
  }
}

/**
 * Supports marshalling to and from a byte array.
 *
 * @author dramage
 * @author dlwh
 */
trait ByteSerialization extends SerializationFormat {
  /** Marshalls the object to a byte array. */
  def toBytes[T:Writable](value : T) : Array[Byte];

  /** Unmarshalls the object from a byte array. */
  def fromBytes[T:Readable](bytes : Array[Byte]) : T;
}

/**
 * Supports marshalling to and from a String.  See TextSerialization
 * for the canonical interface.
 * 
 * @author dramage
 * @author dlwh
 */
trait StringSerialization extends SerializationFormat {
  /** Marshalls the given value as a string. */
  def toString[T:Writable](value: T) : String;

  /** Demarshalls a value from the given string. */
  def fromString[T:Readable](str: String) : T;
}

class SerializationException(msg : String)
extends RuntimeException(msg);
