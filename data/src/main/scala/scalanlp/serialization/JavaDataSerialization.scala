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
import java.io.{ByteArrayInputStream,ByteArrayOutputStream}
import java.io.{DataInput,DataInputStream,DataOutput,DataOutputStream}
import java.io.{File,FileInputStream,FileOutputStream}
import java.io.{InputStream,OutputStream}
import java.io.{ObjectInputStream,ObjectOutputStream}

import scala.collection.mutable.Builder;

/**
 * File-backed Serialization with standard combinators using optimized
 * binary formats where able, default Java serialization otherwise.
 * 
 * @author dlwh
 */
object JavaDataSerialization extends SerializationFormat
with SerializationFormat.PrimitiveTypes with SerializationFormat.CompoundTypes
with ByteSerialization with FileSerialization {
  type Input = DataInput;
  type Output = DataOutput;

  //
  // From ByteSerialization
  // 

  /**
   * Marshalls the object using the implicit Handler to a byte array
   * Usage: JavaDataSerialization.toBytes(myData);
   */
  override def toBytes[T:Writable](x: T) = {
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
  override def fromBytes[T:Readable](bytes: Array[Byte]) = {
    val in = new DataInputStream(new ByteArrayInputStream(bytes));
    val x = implicitly[Readable[T]].read(in);
    in.close;
    x;
  }
  
  //
  // From FileSerialization
  // 
  override def openInput(f: File): DataInput =
    new DataInputStream(new BufferedInputStream(new FileInputStream(f)));

  override def openOutput(f: File): DataOutput =
    new DataOutputStream(new BufferedOutputStream(new FileOutputStream(f)));

  override def closeInput(i: Input) = i match {
    case i: InputStream  => i.close();
    case _ =>
  }

  override def closeOutput(o: Output) = o match {
    case o: OutputStream  => o.close();
    case _ =>
  }

  //
  // From CompoundTypes
  //

  override protected def readBuildable[T:Readable,To]
  (src : Input, builder : Builder[T,To]) : To = {
    val sz = src.readInt;
    for(i <- 0 until sz) {
      builder += implicitly[Readable[T]].read(src);
    }
    builder.result;
  }

  override protected def writeIterable[T:Writable,CC<:Iterable[T]]
  (sink : Output, coll : CC, name : String) {
    sink.writeInt(coll.size);
    for (e <- coll) implicitly[Writable[T]].write(sink,e);
  }

  //
  // From PrimitiveTypes
  //

  override implicit val intReadWritable = new ReadWritable[Int] {
    def read(in: DataInput) = in.readInt();
    def write(out: DataOutput, t: Int) = out.writeInt(t);
  }

  override implicit val byteReadWritable = new ReadWritable[Byte] {
    def read(in: DataInput) = in.readByte();
    def write(out: DataOutput, t: Byte) = out.writeByte(t);
  }

  override implicit val longReadWritable = new ReadWritable[Long] {
    def read(in: DataInput) = in.readLong();
    def write(out: DataOutput, t: Long) = out.writeLong(t);
  }

  override implicit val shortReadWritable = new ReadWritable[Short] {
    def read(in: DataInput) = in.readShort();
    def write(out: DataOutput, t: Short) = out.writeShort(t);
  }

  override implicit val doubleReadWritable = new ReadWritable[Double] {
    def read(in: DataInput) = in.readDouble();
    def write(out: DataOutput, t: Double) = out.writeDouble(t);
  }

  override implicit val floatReadWritable = new ReadWritable[Float] {
    def read(in: DataInput) = in.readFloat();
    def write(out: DataOutput, t: Float) = out.writeFloat(t);
  }

  override implicit val charReadWritable = new ReadWritable[Char] {
    def read(in: DataInput) = in.readChar();
    def write(out: DataOutput, t: Char) = out.writeChar(t);
  }

  override implicit val stringReadWritable = new ReadWritable[String] {
    def read(in: DataInput) = in.readUTF();
    def write(out: DataOutput, t: String) = out.writeUTF(t);
  }

  override implicit val booleanReadWritable = new ReadWritable[Boolean] {
    def read(in: DataInput) = in.readBoolean;
    def write(out: DataOutput, b: Boolean) = out.writeBoolean(b)
  }

  //
  // Builtins
  //

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
