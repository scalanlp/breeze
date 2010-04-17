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

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.DataInput
import java.io.DataInputStream
import java.io.DataOutput
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.InputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.OutputStream
import scala.collection.MapLike
import scala.collection.generic.{TraversableFactory, GenericCompanion, MapFactory};import scalanlp.util.Index


class SerFile(path : String) extends java.io.File(path);

class TxtFile(path : String) extends java.io.File(path);

class CsvFile(path : String) extends java.io.File(path);

/**
 * A base trait for brokers of serialization. See JavaDataSerialization for a good example.
 *
 * Implementors should provide member types Input and Output which are the places the data
 * can be stored. Readable and Writable implicit instances should be created for types
 * that can be serialized in this format. If you also inherit from StandardCombinators,
 * you get a lot of functionality for almost free.
 *
 * @author dlwh
 */
trait SerializationFormat {
  /**
   * The place to read data from
   */
  type Input;
  /**
   * The place to write data to
   */
  type Output;
  trait Readable[T] {
    def read(source: Input): T
  }
  trait Writable[T] {
    def write(sink: Output, what: T):Unit
  }

  /**
   * A convenient wrapper
   */
  trait ReadWritable[T] extends Readable[T] with Writable[T];

  /**
   * Sugar for implicitly[Readable[T]].read(source);
   */
  def read[T:Readable](source: Input): T = implicitly[Readable[T]].read(source);
  /**
   * Sugar for implicitly[Writable[T]].write(sink,what);
   */
  def write[T:Writable](sink: Output, what:T):Unit = implicitly[Writable[T]].write(sink, what);

  /**
   * Implementors should provide a way of creating an Input instance from a java.util.File
   */
  def inputFromFile(f: File): Input;
  /**
   * Implementors should provide a way of creating an Output instance from a java.util.File
   */
  def outputFromFile(f: File): Output;
  /**
   * Implementors should provide a way to close an Input instance. May do nothing, if it's not
   * necessary
   */
  def closeInput(i: Input);
  /**
   * Implementors should provide a way to close an Output instance. May do nothing, if it's not
   * necessary.
   */
  def closeOutput(o: Output);
}

object JavaDataSerialization extends SerializationFormat with StandardCombinators {
  type Input = DataInput;
  type Output = DataOutput;

  def inputFromFile(f: File): DataInput = {
    new DataInputStream(new BufferedInputStream(new FileInputStream(f)));
  }
  def outputFromFile(f: File): DataOutput = {
    new DataOutputStream(new BufferedOutputStream(new FileOutputStream(f)));
  }

  def closeInput(i: Input) = i match {
    case  i: InputStream  => i.close();
    case _ =>
  }

  def closeOutput(i: Output) = i match {
    case  i: OutputStream  => i.close();
    case _ =>
  }

  /**
  * Marshalls the object using the implicit Handler to a byte array
  * Usage: JavaDataSerialization.toBytes(myData);
  */
  def toBytes[T:Writable](x: T) = {
    val bout = new ByteArrayOutputStream();
    val out = new DataOutputStream(bout);
    implicitly[Writable[T]].write(out,x);
    out.close;
    bout.toByteArray;
  }

  /**
  * Unmarshalls the object using the implicit Handler
  * Usage: JavaDataSerialization.fromBytes[T](bytes);
  */
  def fromBytes[T:Readable](bytes: Array[Byte]) = {
    val in = new DataInputStream(new ByteArrayInputStream(bytes));
    val x = implicitly[Readable[T]].read(in);
    in.close;
    x;
  }

  protected def readInt(src: DataInput) = src.readInt;
  protected def writeInt(sink: DataOutput, y: Int) = sink.writeInt(y);
  protected def collectionFromElements[T:ReadWritable,CC[T]<:Iterable[T]](c: GenericCompanion[CC]) = new ReadWritable[CC[T]] {
    def read(source: DataInput) = {
      val sz = source.readInt;
      val b = c.newBuilder[T];
      for(i <- 0 until sz) {
        b += implicitly[Readable[T]].read(source);
      }
      b.result;
    }

    def write(sink: DataOutput, what: CC[T]) {
      sink.writeInt(what.size);
      for(e <- what) {
        implicitly[Writable[T]].write(sink,e);
      }
    }
  }

  protected def collectionFromElements[K:ReadWritable,V:ReadWritable,CC[K,V]<:Map[K,V] with MapLike[K,V,CC[K,V]]](c: MapFactory[CC]) = new ReadWritable[CC[K,V]] {
    def read(source: DataInput) = {
      val sz = source.readInt;
      val b = c.newBuilder[K,V];
      for(i <- 0 until sz) {
        b += implicitly[Readable[(K,V)]](tuple2ReadWritable).read(source);
      }
      b.result;
    }

    def write(sink: DataOutput, what: CC[K,V]) {
      sink.writeInt(what.size);
      for( (e:(K,V)) <- what) {
        implicitly[Writable[(K,V)]].write(sink,e);
      }
    }

  }

  implicit val doubleReadWritable = new ReadWritable[Double] {
    def read(in: DataInput) = in.readDouble();
    def write(out: DataOutput, t: Double) = out.writeDouble(t);
  }

  implicit val intReadWritable = new ReadWritable[Int] {
    def read(in: DataInput) = in.readInt();
    def write(out: DataOutput, t: Int) = out.writeInt(t);
  }

  implicit val floatReadWritable = new ReadWritable[Float] {
    def read(in: DataInput) = in.readFloat();
    def write(out: DataOutput, t: Float) = out.writeFloat(t);
  }

  implicit val byteReadWritable : ReadWritable[Byte] = new ReadWritable[Byte] {
    def read(in: DataInput) = in.readByte();
    def write(out: DataOutput, t: Byte) = out.writeByte(t);
  }

  implicit val longReadWritable = new ReadWritable[Long] {
    def read(in: DataInput) = in.readLong();
    def write(out: DataOutput, t: Long) = out.writeLong(t);
  }

  implicit val shortReadWritable: ReadWritable[Short] = new ReadWritable[Short] {
    def read(in: DataInput) = in.readShort();
    def write(out: DataOutput, t: Short) = out.writeShort(t);
  }

  implicit val charReadWritable:ReadWritable[Char] = new ReadWritable[Char] {
    def read(in: DataInput) = in.readChar();
    def write(out: DataOutput, t: Char) = out.writeChar(t);
  }

  implicit val stringReadWritable = new ReadWritable[String] {
    def read(in: DataInput) = {
      in.readUTF();
    }
    def write(out: DataOutput, t: String) = {
      out.writeUTF(t);
    }
  }

  implicit val booleanReadWritable = new ReadWritable[Boolean] {
    def read(in: DataInput) = in.readBoolean;
    def write(out: DataOutput, b: Boolean) = out.writeBoolean(b)
  }

  implicit val byteArrayReadWritable = arrayReadWritable[Byte];

  /**
   * Uses Java serialization. It's *very* inefficient, and should be avoided.
   */
  def naiveReadWritable[T] = new ReadWritable[T] {
    def read(in: DataInput) = {
      val ba = byteArrayReadWritable read in;
      val oin = new ObjectInputStream(new ByteArrayInputStream(ba));
      val x = oin.readObject().asInstanceOf[T];
      oin.close;
      x;
    }

    def write(out: DataOutput, x: T) {
      val bout = new ByteArrayOutputStream();
      val oout = new ObjectOutputStream(bout);
      oout.writeObject(x);
      oout.close;
      byteArrayReadWritable.write(out, bout.toByteArray);
    }
  }
}

trait StandardCombinators { this: SerializationFormat =>

  protected def readInt(src: Input):Int;
  protected def writeInt(sink: Output, t: Int):Unit;
  protected def collectionFromElements[T:ReadWritable,CC[T]<:Iterable[T]](c: GenericCompanion[CC]):ReadWritable[CC[T]];
  protected def collectionFromElements[K:ReadWritable,V:ReadWritable,CC[K,V]<:Map[K,V] with MapLike[K,V,CC[K,V]]](c: MapFactory[CC]): ReadWritable[CC[K,V]];

  implicit def tuple3ReadWritable[T1,T2,T3](implicit t1H: ReadWritable[T1], t2H: ReadWritable[T2], t3H: ReadWritable[T3]) = new ReadWritable[(T1,T2,T3)] {
    def read(in: Input) = {
      val t1 = t1H.read(in);
      val t2 = t2H.read(in);
      val t3 = t3H.read(in);
      (t1,t2,t3)
    }
    def write(out: Output, t: (T1,T2,T3)) {
      t1H.write(out, t._1);
      t2H.write(out, t._2);
      t3H.write(out, t._3);
    }
  }

  implicit def tuple2ReadWritable[T1,T2](implicit t1H: ReadWritable[T1], t2H: ReadWritable[T2]) = new ReadWritable[(T1,T2)] {
    def read(in: Input) = {
      val t1 = t1H.read(in);
      val t2 = t2H.read(in);
      (t1,t2)
    }
    def write(out: Output, t: (T1,T2)) {
      t1H.write(out, t._1);
      t2H.write(out, t._2);
    }
  }

  implicit def tuple4ReadWritable[T1,T2,T3,T4](implicit t1H: ReadWritable[T1],
                                               t2H: ReadWritable[T2],
                                               t3H: ReadWritable[T3],
                                               t4H: ReadWritable[T4]) = new ReadWritable[(T1,T2,T3,T4)] {
    def read(in: Input) = {
      val t1 = t1H.read(in);
      val t2 = t2H.read(in);
      val t3 = t3H.read(in);
      val t4 = t4H.read(in);
      (t1,t2,t3,t4)
    }
    def write(out: Output, t: (T1,T2,T3,T4)) {
      t1H.write(out, t._1);
      t2H.write(out, t._2);
      t3H.write(out, t._3);
      t4H.write(out, t._4);
    }
  }


  implicit def arrayReadWritable[T](implicit tH: ReadWritable[T], man: ClassManifest[T]) = new ReadWritable[Array[T]] {
    def read(in: Input) = {
      val sz = readInt(in);
      Array.tabulate(sz) { i =>
        tH read in;
      }
    }

    def write(o: Output, x: Array[T]) {
      writeInt(o,x.size);
      x foreach { tH.write(o,_) }
    }
  }

  implicit def listReadWritable[T](implicit tH: ReadWritable[T]) = collectionFromElements[T,List](List);

  implicit def seqReadWritable[T](implicit tH: ReadWritable[T]) = collectionFromElements[T,Seq](Seq);

  implicit def imSetReadWritable[T](implicit tH: ReadWritable[T]) = collectionFromElements[T,Set](Set);

  implicit def imMapReadWritable[K:ReadWritable,V:ReadWritable] = collectionFromElements[K,V,Map](Map);

  implicit def indexReadWritable[T:ReadWritable] = new ReadWritable[Index[T]] {
    def read(in: Input):Index[T] = {
      val sz = readInt(in);
      val col = Index[T]();
      for(i <- 0 until sz) {
        col.index(implicitly[ReadWritable[T]].read(in));
      }
      col
    }

    def write(out: Output, ind: Index[T]) {
      writeInt(out,ind.size);
      for(i <- 0 until ind.size) {
        implicitly[ReadWritable[T]].write(out,ind.get(i))
      }
    }
  }


}