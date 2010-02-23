/*
 * Distributed as part of ScalaRA, a scientific research tool.
 *
 * Copyright (C) 2007 Daniel Ramage
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA
 */
package scalara.serializer;

import java.io.{DataInputStream,DataOutputStream};


/**
 * Represents a builder for constructing instances of Repr from the
 * given Backing.
 *
 * @author dramage
 */
trait Loadable[Repr,Backing] {
  def load(source : Backing) : Repr;
}

object Loadable {
  def apply[Repr,Backing](f : (Backing => Repr)) : Loadable[Repr,Backing] = {
    new Loadable[Repr,Backing] {
      override def load(source : Backing) = f(source);
    }
  }

  //
  // Primitive types
  //

  implicit def iStream2Double = Loadable[Double,DataInputStream](
    (source : DataInputStream) => source.readDouble());

  implicit def iStream2Float = Loadable[Float,DataInputStream](
    (source : DataInputStream) => source.readFloat());

  implicit def iStream2Int = Loadable[Int,DataInputStream](
    (source : DataInputStream) => source.readInt());

  implicit def iStream2Short = Loadable[Short,DataInputStream](
    (source : DataInputStream) => source.readShort());

  implicit def iStream2Long = Loadable[Long,DataInputStream](
    (source : DataInputStream) => source.readLong());

  implicit def iStream2Char = Loadable[Char,DataInputStream](
    (source : DataInputStream) => source.readChar());

  implicit def iStream2Boolean = Loadable[Boolean,DataInputStream](
    (source : DataInputStream) => source.readBoolean());

  //
  // Array types
  //

  implicit def iStream2Array[T](implicit loadable : Loadable[T,DataInputStream], m : ClassManifest[T]) = {
    Loadable[Array[T],DataInputStream] {
      (source : DataInputStream) =>
      Array.tabulate(source.readInt)(i => loadable.load(source));
    }
  }

//  implicit def iStream2Buildable[T,Coll[_]](implicit loadable : Loadable[T,DataInputStream], builder : scala.collection.mutable.Builder[T,Coll[T]]) = {
//    Loadable[Coll[T],DataInputStream] {
//      (source : DataInputStream) => {
//        for (i <- 0 until source.readInt) {
//          builder += loadable.load(source);
//        }
//        builder.result;
//      }
//    }
//  }
}

/**
 * Represents a builder for saving an instance of Repr to the given
 * Backing.
 *
 * @author dramage
 */
trait Saveable[Repr,Backing] {
  def save(value : Repr, target : Backing) : Unit;
}

object Saveable {
  def apply[Repr,Backing](f : (Repr, Backing) => Unit) : Saveable[Repr,Backing] = {
    new Saveable[Repr,Backing] {
      override def save(value : Repr, target : Backing) = f(value, target);
    }
  }

  //
  // Primitive types
  //

  implicit def iDouble2Stream = Saveable[Double,DataOutputStream](
    (value : Double, target : DataOutputStream) => target.writeDouble(value));

  implicit def iFloat2Stream = Saveable[Float,DataOutputStream](
    (value : Float, target : DataOutputStream) => target.writeFloat(value));

  implicit def iInt2Stream = Saveable[Int,DataOutputStream](
    (value : Int, target : DataOutputStream) => target.writeInt(value));

  implicit def iShort2Stream = Saveable[Short,DataOutputStream](
    (value : Short, target : DataOutputStream) => target.writeShort(value));

  implicit def iLong2Stream = Saveable[Long,DataOutputStream](
    (value : Long, target : DataOutputStream) => target.writeLong(value));

  implicit def iChar2Stream = Saveable[Char,DataOutputStream](
    (value : Char, target : DataOutputStream) => target.writeChar(value));

  implicit def iBoolean2Stream = Saveable[Boolean,DataOutputStream](
    (value : Boolean, target : DataOutputStream) => target.writeBoolean(value));

  //
  // Array types
  //

  implicit def iArray2Stream[T](implicit saveable : Saveable[T,DataOutputStream], m : ClassManifest[T]) = {
    Saveable[Array[T],DataOutputStream] {
      (value : Array[T], target : DataOutputStream) => {
        target.writeInt(value.length);
        for (v <- value) saveable.save(v,target);
      }
    }
  }

//  implicit def iIterable2Stream[T](implicit saveable : Saveable[T,DataOutputStream], m : ClassManifest[T]) = {
//    Saveable[Iterable[T],DataOutputStream] {
//      (value : Iterable[T], target : DataOutputStream) => {
//        target.writeInt(value.size);
//        for (v <- value) saveable.save(v,target);
//      }
//    }
//  }
}

/**
 * Static methods.
 *
 * @author dramage
 */
object Serializer {
  def save[Repr,Backing](value : Repr, target : Backing)
  (implicit saveable : Saveable[Repr,Backing]) = {
    saveable.save(value, target);
  }

  def load[Repr,Backing](source : Backing)
  (implicit loadable : Loadable[Repr,Backing]) = {
    loadable.load(source);
  }
}


object SerializerTest {
  import Serializer._;

  def loop[T](value : T)
  (implicit saveable : Saveable[T,DataOutputStream],
   loadable : Loadable[T,DataInputStream])
  : T = {
    val bos = new java.io.ByteArrayOutputStream(1024)
    Serializer.save(value, new DataOutputStream(bos));

    val bis = new java.io.ByteArrayInputStream(bos.toByteArray);
    Serializer.load[T,DataInputStream](new DataInputStream(bis));
  }

  def main(args : Array[String]) {
    println(3.14 + " " + loop(3.14));
    println(27 + " " + loop(27));
    println(Array(1.0,2.0,3.0).deep + " " + loop(Array(1.0,2.0,3.0)).deep);
    // println(List(1,2,3) + " " + loop(List(1,2,3)));
  }
}
