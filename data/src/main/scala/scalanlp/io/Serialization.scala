package scalanlp.io;
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

import java.io._;
import java.util.zip._;

/**
* Provides rudimentary serialization for scala types.
* Inspired by SBinary, which seems kind of defunct.
*/
object Serialization {
  @serializable
  trait Handler[T] {
    def read(in: DataInput):T
    def write(t:T, out: DataOutput): Unit;
  }

  trait VersionedHandler[T] extends Handler[T] {
    def currentVersion: Long
    final def read(in: DataInput): T = {
      val ver = in.readLong;
      read(ver,in);
    }

    def read(version: Long, in: DataInput):T;

    final def write(t: T, out: DataOutput) {
      out.writeLong(currentVersion);
      writeCurrent(t,out);
    }

    def writeCurrent(t: T, out: DataOutput):Unit;
  }

  object Handlers {
    implicit val doubleHandler = new Handler[Double] {
      def read(in: DataInput) = in.readDouble();
      def write(t: Double, out: DataOutput) = out.writeDouble(t);
    }

    implicit val intHandler = new Handler[Int] {
      def read(in: DataInput) = in.readInt();
      def write(t: Int, out: DataOutput) = out.writeInt(t);
    }

    implicit val floatHandler = new Handler[Float] {
      def read(in: DataInput) = in.readFloat();
      def write(t: Float, out: DataOutput) = out.writeFloat(t);
    }

    implicit val byteHandler : Handler[Byte] = new Handler[Byte] {
      def read(in: DataInput) = in.readByte();
      def write(t: Byte, out: DataOutput) = out.writeByte(t);
    }

    implicit val longHandler = new Handler[Long] {
      def read(in: DataInput) = in.readLong();
      def write(t: Long, out: DataOutput) = out.writeLong(t);
    }

    implicit val shortHandler: Handler[Short] = new Handler[Short] {
      def read(in: DataInput) = in.readShort();
      def write(t: Short, out: DataOutput) = out.writeShort(t);
    }

    implicit val charHandler:Handler[Char] = new Handler[Char] {
      def read(in: DataInput) = in.readChar();
      def write(t: Char, out: DataOutput) = out.writeChar(t);
    }

    implicit val stringHandler = new Handler[String] {
      def read(in: DataInput) = {
        in.readUTF();
      }
      def write(t: String, out: DataOutput) = {
          out.writeUTF(t);
      }
    }

    implicit val booleanHandler = new Handler[Boolean] {
      def read(in: DataInput) = in.readBoolean;
      def write(b: Boolean, out: DataOutput) = out.writeBoolean(b)
    }

    implicit def tuple2Handler[T1,T2](implicit t1H: Handler[T1], t2H: Handler[T2]) = new Handler[(T1,T2)] {
      def read(in: DataInput) = {
        val t1 = t1H.read(in);
        val t2 = t2H.read(in);
        (t1,t2)
      }
      def write(t: (T1,T2), out: DataOutput) {
        t1H.write(t._1, out);
        t2H.write(t._2, out);
      }
    }

    implicit def tuple3Handler[T1,T2,T3](implicit t1H: Handler[T1], t2H: Handler[T2], t3H: Handler[T3]) = new Handler[(T1,T2,T3)] {
      def read(in: DataInput) = {
        val t1 = t1H.read(in);
        val t2 = t2H.read(in);
        val t3 = t3H.read(in);
        (t1,t2,t3)
      }
      def write(t: (T1,T2,T3), out: DataOutput) {
        t1H.write(t._1, out);
        t2H.write(t._2, out);
        t3H.write(t._3, out);
      }
    }

    implicit def tuple4Handler[T1,T2,T3,T4](implicit t1H: Handler[T1],
                                            t2H: Handler[T2],
                                            t3H: Handler[T3],
                                            t4H: Handler[T4]) = new Handler[(T1,T2,T3,T4)] {
      def read(in: DataInput) = {
        val t1 = t1H.read(in);
        val t2 = t2H.read(in);
        val t3 = t3H.read(in);
        val t4 = t4H.read(in);
        (t1,t2,t3,t4)
      }
      def write(t: (T1,T2,T3,T4), out: DataOutput) {
        t1H.write(t._1, out);
        t2H.write(t._2, out);
        t3H.write(t._3, out);
        t4H.write(t._4, out);
      }
    }

    implicit def arrayHandler[T](implicit tH: Handler[T], man: ClassManifest[T]) = new Handler[Array[T]] {
      def read(in: DataInput) = {
        val sz = in.readInt;
        Array.tabulate(sz) { i => 
        tH read in;
        }
      }

      def write(x: Array[T], o: DataOutput) {
        o writeInt x.size;
        x foreach { tH.write(_,o) }
      }
    }

    import Builders._;
    implicit def listHandler[T](implicit tH: Handler[T]) = collectionFromElements[T,List[T]](_.toList);

    implicit def seqHandler[T](implicit tH: Handler[T]) = collectionFromElements[T,Seq[T]](_.toSeq);

    implicit def imSetHandler[T](implicit tH: Handler[T]) = collectionFromElements[T,Set[T]](Set() ++ _);

    implicit def mSetHandler[T](implicit tH: Handler[T]) = collectionFromElements[T,scala.collection.mutable.Set[T]]{ elems => 
      scala.collection.mutable.Set() ++= elems
    };

    implicit def imMapHandler[K,V](implicit h: Handler[(K,V)]) = collectionFromElements[(K,V),Map[K,V]](Map() ++ _);

    implicit def mMapHandler[K,V](implicit h: Handler[(K,V)]) = {
      collectionFromElements[(K,V),scala.collection.mutable.Map[K,V]]{ elems => 
        scala.collection.mutable.Map() ++= elems
      };
    }
  }

  /**
  * Contains methods to make Handlers from other handlers
  */
  object Builders {
    import Handlers._;
    /**
    * Serializes the elements of the collection, and builds the collection back 
    * using inflate, which must create a collection from the members.
    */
    def collectionFromElements[T:Handler,C<:Iterable[T]](inflate: Iterator[T]=>C):Handler[C] = new Handler[C] {
      def write(c: C, out: DataOutput) = {
        Serialization.write(c.size,out);
        for(e <- c) {
          Serialization.write(e,out);
        }
      }

      def read(in: DataInput) = {
        val sz = Serialization.read[Int](in);
        val elems = for( i <- Iterator.range(0,sz) ) yield { 
          val a = Serialization.read[T](in);
          a
        }
        inflate(elems);
      }
    }

    private val byteArrayHandler = Handlers.arrayHandler[Byte](Handlers.byteHandler,reflect.Manifest.Byte);

    /**
    * Uses Java serialization. It's *very* inefficient, and should be avoided.
    */
    def naiveHandler[T] = new Handler[T] {
      def read(in: DataInput) = {
        val ba = byteArrayHandler read in;
        val oin = new ObjectInputStream(new ByteArrayInputStream(ba));
        val x = oin.readObject().asInstanceOf[T];
        oin.close;
        x;
      }

      def write(x: T, out: DataOutput) { 
        val bout = new ByteArrayOutputStream();
        val oout = new ObjectOutputStream(bout);
        oout.writeObject(x);
        oout.close;
        byteArrayHandler.write(bout.toByteArray,out);
      }
    }

  }

  def write[T:Handler](x: T, o: DataOutput) {
    implicitly[Handler[T]].write(x,o);
  }


  def read[T:Handler](i: DataInput) = {
    implicitly[Handler[T]].read(i);
  }


  def writeToFile[T](x: T, f: File)(implicit h: Handler[T]) {
    val out = new DataOutputStream(new FileOutputStream(f));
    h.write(x,out);
    out.close();
  }

  def readFromFile[T](f: File)(implicit h: Handler[T]) = {
    val in = new DataInputStream(new FileInputStream(f));
    val r = h.read(in);
    in.close();
    r
  }

  /**
  * Marshalls the object using the implicit Handler to a byte array
  */
  def toBytes[T](x: T)(implicit h: Handler[T]) = {
    val bout = new ByteArrayOutputStream();
    val out = new DataOutputStream(bout);
    h.write(x,out);
    out.close;
    bout.toByteArray;
  }

  /**
  * Unmarshalls the object using the implicit Handler
  * Usage: Serialization.fromBytes[T](bytes);
  */
  def fromBytes[T](bytes: Array[Byte])(implicit h: Handler[T]) = {
    val in = new DataInputStream(new ByteArrayInputStream(bytes));
    val x = h.read(in);
    in.close;
    x;
  }

  object ScalanlpHandlers {
    import scalala.tensor.counters.Counters._;
    import Serialization.Handlers._;
    import Builders._;
    implicit def doubleCounterHandler[T](implicit h: Handler[T]) = new VersionedHandler[DoubleCounter[T]] {
      def currentVersion = 3L;
      def read(v: Long, in: DataInput) = {
        val sz = in.readInt;

        val c = DoubleCounter[T]();
        val elems = (1 to sz) map {_ => 
          val k = h read in;
          val v = doubleHandler read in
          (k,v)
        } iterator;
        c ++= elems;
        c
      }

      def writeCurrent(c: DoubleCounter[T], out: DataOutput) = {
        out writeInt c.size;
        for((k,v) <- c) {
          h.write(k,out);
          doubleHandler.write(v,out);
        }
      }
    }

    import scalanlp.util._;
    implicit def indexHandler[T](implicit h: Handler[T]) = collectionFromElements[T,Index[T]] { elems =>
      val ind = Index[T]();
      elems foreach {s =>  ind.index(s)};
      ind
    }
  }

}
