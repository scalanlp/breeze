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

import scala.collection.MapLike
import scala.collection.generic.{GenericCompanion, MapFactory}

import scala.collection.mutable.{ArrayBuilder,Builder};

import scalanlp.util.Index

/**
 * Reads type V from input Input.
 *
 * @author dramage
 */
trait Readable[-Input,@specialized V] {
  def read(input : Input) : V;
}

/**
 * Writes type V to Output.
 *
 * @author dramage
 */
trait Writable[-Output,@specialized V] {
  def write(output : Output, value : V);
}

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
@serializable trait SerializationFormat {
  /** The place to read data from. */
  type Input;

  /** The place to write data to. */
  type Output;

  /** Inner trait for reading from Input. */
  @serializable trait Readable[@specialized T] {
    def read(source: Input): T
    
    /** Returns true if this readable requires streaming. */
    def streaming : Boolean = false;
  }

  /** Inner trait for writing to Output. */
  @serializable trait Writable[@specialized T] {
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
  trait CompoundTypes { self : SerializationFormat with PrimitiveTypes =>

    protected def readName(in : Input) : String;

    protected def writeName(out : Output, name : String);

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

    implicit def iteratorReadWritable[T](implicit tH: ReadWritable[T])
    = new ReadWritable[Iterator[T]] {
      override def streaming = true;
    
      def read(source : Input) = new Iterator[T] {
        var pending = self.read[Int](source);
        override def hasNext = pending > 0;
        override def next = {
          require(pending > 0);
          val rv = tH.read(source);
          pending -= 1;
          if (pending == 0) {
            pending = self.read[Int](source);
            if (pending == 0) {
              // close source once there's a hook ...
              // source.close;
            }
          }
          rv;
        }
      }
      
      def write(out : Output, iter : Iterator[T]) {
        for (group <- iter.grouped(10)) {
          self.write(out, group.length);
          for (item <- group) tH.write(out, item);
        }
        self.write(out, 0);
      }
    }
      
    implicit def listReadWritable[T](implicit tH: ReadWritable[T]) =
      collectionFromElements[T,List](List,"List");

    implicit def seqReadWritable[T](implicit tH: ReadWritable[T]):ReadWritable[Seq[T]] =
      collectionFromElements[T,Seq](Seq,"Seq");

    implicit def indexedSeqReadWritable[T](implicit tH: ReadWritable[T]) =
      collectionFromElements[T,IndexedSeq](IndexedSeq,"IndexedSeq");

    implicit def setReadWritable[T](implicit tH: ReadWritable[T]) =
      collectionFromElements[T,Set](Set,"Set");

    implicit def mapReadWritable[K:ReadWritable,V:ReadWritable] =
      collectionFromElements[K,V,Map](Map,"Map");


    implicit def iterableReadWritable[T](implicit tH: ReadWritable[T]) =
      collectionFromElements[T,Iterable](Iterable,"Iterable");

    implicit def indexReadWritable[T:ReadWritable]
    = new ReadWritable[Index[T]] {
      def read(source: Input): Index[T] =
        Index(seqReadWritable[T].read(source));

      def write(sink: Output, value: Index[T]) =
        writeIterable[T,Iterable[T]](sink, value, "Index");
    }

    implicit def optionReadWritable[T:ReadWritable] : ReadWritable[Option[T]]
    = new ReadWritable[Option[T]] {
      override def read(in : Input) = {
        readName(in) match {
          case "Some" =>
            readTupleStart(in);
            val rv = implicitly[ReadWritable[T]].read(in);
            readTupleEnd(in);
            Some(rv);
          case "None" =>
            None;
        }
      }

      override def write(out : Output, option : Option[T]) = {
        option match {
          case Some(v) =>
            writeName(out, "Some");
            writeTupleStart(out);
            implicitly[ReadWritable[T]].write(out, v);
            writeTupleEnd(out);
          case None =>
            writeName(out, "None");
        }
      }
    }

    /**
     * Constructable provides a simple way to add serialization support to
     * a more basic type.  In a companion object, extend a SerializationFormat's
     * Constructable as an implicit object.
     *
     * @author dramage
     */
    abstract class Constructible[V:ClassManifest,RW:ReadWritable] extends ReadWritable[V] {
      /** Name written and read. */
      def name : String;

      /** Packs the given value into a representation. */
      def pack(value : V) : RW;

      /** Unpacks the given value from a representation. */
      def unpack(rep : RW) : V;

      override def read(in : Input) = {
        val seen = readName(in);
        if (seen != name) {
          throw new SerializationException("Expected: "+name+" but got "+seen);
        }
        unpack(implicitly[ReadWritable[RW]].read(in));
      }

      override def write(out : Output, value : V) = {
        writeName(out,name);
        implicitly[ReadWritable[RW]].write(out,pack(value));
      }
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
 * Abstract serialization format that supports marshalling to and from a String.
 * TextSerialization extends this trait with functionality to actually read
 * and write values as text..
 * 
 * @author dramage
 * @author dlwh
 */
trait StringSerialization extends SerializationFormat with ByteSerialization {
  /** Encoding used by this StringSerialization instance.  Defaults to UTF8, independent of platform. */
  def encoding = "UTF8";

  /** Marshalls the given value as a string. */
  def toString[T:Writable](value: T) : String;

  /** Demarshalls a value from the given string. */
  def fromString[T:Readable](str: String) : T;

  /** Returns a byte array using the this.encoding as the byte encoding of the value returned by toString. */
  override def toBytes[T:Writable](x: T) =
    toString(x).getBytes(encoding);

  /** Returns fromString called on the string created using the this.encoding as the byte encoding of given bytes. */
  override def fromBytes[T:Readable](bytes: Array[Byte]) : T =
    fromString[T](new String(bytes,encoding));
}

class SerializationException(msg : String)
extends RuntimeException(msg);
