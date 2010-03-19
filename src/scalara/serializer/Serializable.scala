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

import scala.collection.generic.{TraversableFactory,GenericTraversableTemplate};
import scala.collection.mutable.{Builder};

import java.io.{DataInputStream,DataOutputStream};

class SerFile(path : String) extends java.io.File(path);

class TxtFile(path : String) extends java.io.File(path);

class CsvFile(path : String) extends java.io.File(path);

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
  // File backings
  //

  implicit def iSerFile2Serializable[X] : Loadable[X,SerFile] = {
    import java.io.FileInputStream;
    import java.io.ObjectInputStream;

    Loadable[X,SerFile]((source : SerFile) => {
      val stream = new ObjectInputStream(new FileInputStream(source));
      val rv = try { stream.readObject.asInstanceOf[X]; } finally { stream.close; }
      rv;
    });
  }

  implicit def iTxtFile2Items[X](implicit conversion : (String => X))
  : Loadable[Iterable[X],TxtFile] = {
    import java.io.FileInputStream;

    Loadable[Iterable[X],TxtFile]((source : TxtFile) =>
      new Iterable[X] {
        override def iterator =
          scalara.pipes.PipeIO.readLines(new FileInputStream(source)).map(conversion);

        override def toString =
          source.toString;
      }
    );
  }

//  implicit def iTxtFile2Traversable[Item,CC[X]<:Traversable[X] with GenericTraversableTemplate[X,CC]]
//  (implicit conversion : (String => Item), companion : TraversableFactory[CC])
//  : Loadable[CC[Item],TxtFile] = {
//    import java.io.FileInputStream;
//
//    Loadable[CC[Item],TxtFile]((source : TxtFile) => {
//      val builder = companion.newBuilder[Item];
//      val stream = new FileInputStream(source);
//      try {
//        for (line <- scalara.pipes.PipeIO.readLines(stream)) {
//          builder += conversion(line);
//        }
//        builder.result;
//      } finally {
//        stream.close();
//      }
//    });
//  }
//
//  implicit def iTxtFile2Traversable[String,CC<:Traversable[String]]
//  (implicit builder : Builder[String,CC])
//  : Loadable[CC,TxtFile] = {
//    import java.io.FileInputStream;
//
//    Loadable[CC,TxtFile]((source : TxtFile) => {
//      val stream = new FileInputStream(source);
//      try {
//        for (line <- scalara.pipes.PipeIO.readLines(stream)) {
//          builder += line;
//        }
//        builder.result;
//      } finally {
//        stream.close();
//      }
//    });
//  }

  //
  // DataInputStream
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
  // File backings
  //

  implicit def iSerializable2SerFile[X] : Saveable[X,SerFile] = {
    import java.io.FileOutputStream;
    import java.io.ObjectOutputStream;

    Saveable[X,SerFile]((value : X, target : SerFile) => {
      val stream = new ObjectOutputStream(new FileOutputStream(target));
      try {
        stream.writeObject(value);
      } finally {
        stream.close;
      }
    });
  }

  implicit def iIterable2TxtFile[X](implicit conversion : (X => String))
  : Saveable[Iterable[X],TxtFile] = {
    import java.io.{FileOutputStream,PrintStream};

    Saveable[Iterable[X],TxtFile]((value : Iterable[X], target : TxtFile) => {
      val stream = new PrintStream(new FileOutputStream(target));
      try {
        for (item <- value) {
          stream.println(conversion(item));
        }
      } finally {
        stream.close;
      }
    });
  }

  implicit def iTraversable2TxtFile[Collection<:Traversable[String]]
  : Saveable[Collection,TxtFile] = {
    import java.io.{FileOutputStream,PrintStream};

    Saveable[Collection,TxtFile]((value : Collection, target : TxtFile) => {
      val stream = new PrintStream(new FileOutputStream(target));
      try {
        for (item <- value) {
          stream.println(item);
        }
      } finally {
        stream.close();
      }
    });
  }

  //
  // DataOutputStream
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

  implicit def iArray2Stream[T](implicit saveable : Saveable[T,DataOutputStream], m : ClassManifest[T]) = {
    Saveable[Array[T],DataOutputStream] {
      (value : Array[T], target : DataOutputStream) => {
        target.writeInt(value.length);
        for (v <- value) saveable.save(v,target);
      }
    }
  }
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

  def testTxtFile() {
    val tmpFile = new TxtFile("scalara.tmp");
    val data = List("line 1","line 2", "line 3");
    Serializer.save(data, tmpFile); // (Saveable.iTraversable2TxtFile[String,List[String]]);
    val loaded = Serializer.load[List[String],TxtFile](tmpFile)(Loadable.iTxtFile2Traversable[List[String]]);
    println(data.mkString(" __ "));
    println(loaded.mkString(" __ "));
    if (data sameElements loaded) {
      println("[TestTxtFile] OK");
    } else {
      println("[TestTxtFile] FAIL");
    }
    tmpFile.delete;
  }

  def main(args : Array[String]) {
    println(3.14 + " " + loop(3.14));
    println(27 + " " + loop(27));
    println(Array(1.0,2.0,3.0).deep + " " + loop(Array(1.0,2.0,3.0)).deep);
    testTxtFile();
    // println(List(1,2,3) + " " + loop(List(1,2,3)));
  }
}
