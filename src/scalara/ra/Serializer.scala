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
package scalara.ra;

import java.io.File;

import scala.reflect.Manifest;



object JSON {
  abstract class JSONSerializable {
    def toJSON : Iterator[String];
  }

  object JSONSerializable {
    def apply(json : =>String) = new JSONSerializable {
      override def toJSON = Iterator(json);
    }
  }

  abstract class JSONSerializableDepth2[A<%JSONSerializable] extends JSONSerializable;

  object JSONSerializableDepth2 {
    def apply[A<%JSONSerializable](json : =>String) = new JSONSerializableDepth2[A] {
      override def toJSON = Iterator(json);
    }
  }


  abstract class JSONSerializableDepth3[B,A<%JSONSerializableDepth2[B]] extends JSONSerializableDepth2[A];

  implicit def iToJSONSerializable(str : String) =
    if (str == null) "null" else "\"" + escapeJSON(str) + "\"";

  implicit def iToJSONSerializable(v : Double) = v.toString;
  implicit def iToJSONSerializable(v : Float) = v.toString;
  implicit def iToJSONSerializable(v : Int) = v.toString;
  implicit def iToJSONSerializable(v : Long) = v.toString;
  implicit def iToJSONSerializable(v : Short) = v.toString;
  implicit def iToJSONSerializable(v : Char) = v.toString;
  implicit def iToJSONSerializable(v : Boolean) = v.toString;

  implicit def iToJSONSerializable(v : Array[Double]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Float]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iIntArrayToJSONSerializable(v : Array[Int]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Long]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Short]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Char]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Boolean]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable1[A<%JSONSerializable](v : Array[A]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.iterator.map(_.toJSON).mkString(", ")+"]"; }
  implicit def iToJSONSerializable2[B,A<%JSONSerializable](v : Array[A]) = JSONSerializableDepth2[A] {
    if (v == null) "null" else "[ " + v.iterator.map(_.toJSON).mkString(", ")+"]"; }

  @inline final private def escapeChar(c : Char) : String = c match {
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
    case _ => c.toString;
  }

  def escapeJSON(str : String) : String =
    str.flatMap(escapeChar)

  
  def manifestOf[V](implicit m : Manifest[V]) = m;

  val x = Array(Array(1,2),Array(3),Array(4,5,6));
  // iToJSONSerializable(x)(manifestOf[Array[Int]], iIntArrayToJSONSerializable).toJSON;
  // println(x.toJSON.mkString("\n"));
}


object NewSeralizer {
  import java.io.{InputStream,OutputStream};

  abstract class Loadable[Repr,Backing](implicit ra : RA, m : Manifest[Repr]) {
    def load(source : Backing) : Repr;
  }
  
  abstract class Saveable[Repr,Backing](implicit ra : RA, m : Manifest[Repr]) {
    def save(value : Repr, target : Backing) : Unit;
  }

  abstract class StreamLoadable[Repr,Backing<:InputStream](implicit ra : RA, m : Manifest[Repr])
  extends Loadable[Repr,Backing];

  abstract class StreamSaveable[Repr,Backing<:OutputStream](implicit ra : RA, m : Manifest[Repr])
  extends Saveable[Repr,Backing];

  abstract class LineRecordLoadable[Repr,Backing<:InputStream](implicit ra : RA, m : Manifest[Repr])
  extends StreamLoadable[Repr,Backing] {
    import ra.pipes._;

    def fromStrings(source : Iterator[String]) : Repr;

    override def load(source : Backing) : Repr =
      fromStrings(source.getLines);
  }

  abstract class LineRecordSavable[Repr,Backing<:OutputStream](implicit ra : RA, m : Manifest[Repr])
  extends StreamSaveable[Repr,Backing] {
    import ra.pipes._;

    def toStrings(value : Repr) : Iterator[String];

    def save(value : Repr, target : Backing) =
      toStrings(value) | target;
  }
}


/**
 * Custom flexible file-backed serialization using Scala reflected
 * manifests.
 * 
 * @author dramage
 */
object Serializer {
  /**
   * Responsible for loading types off disk.
   */
  trait Loader {
    def canLoad[V](implicit valType : scala.reflect.Manifest[V]) : Boolean;
    def load[V](file : java.io.File)(implicit valType : scala.reflect.Manifest[V], ra : RA) : V;
  }

  object Loader {
    /** The default loader uses Java's built-in serialization. */
    val default = new Loader {
      override def canLoad[V](implicit valType : scala.reflect.Manifest[V]) : Boolean =
        true;
      
      override def load[V](f : java.io.File)(implicit valType : scala.reflect.Manifest[V], ra : RA) : V = {
        import ra.pipes._;
        ra.log("RA.Serializer: default load for "+valType);
        val ois = new java.io.ObjectInputStream(f);
        val rv = try { ois.readObject } finally { ois.close }
        rv.asInstanceOf[V];
      }
    }
  }

  /**
   * Responsible for saving types to disk.
   */
  trait Saver {
    def canSave[V](implicit valType : scala.reflect.Manifest[V]) : Boolean;
    def save[V](file : java.io.File, value : V)(implicit valType : scala.reflect.Manifest[V], ra : RA) : Unit;
  }
  
  object Saver {
    /** The default saver uses Java's built-in serialization. */
    val default = new Saver {
      override def canSave[V](implicit valType : scala.reflect.Manifest[V]) =
        true;
      
      override def save[V](f : java.io.File, value : V)(implicit valType : scala.reflect.Manifest[V], ra : RA) {
        import ra.pipes._;
        ra.log("RA.Serializer: default save for "+valType);
        val oos = new java.io.ObjectOutputStream(f);
        try { oos.writeObject(value) } finally { oos.close }
      }
    }
  }
  
  
  /** Savers with default implementation that use ObjectOutputStream. */
  protected val savers = new scala.collection.mutable.ArrayBuffer[Saver]();
  
  /** Loaders with default implementation that use ObjectInputStream. */
  protected val loaders = new scala.collection.mutable.ArrayBuffer[Loader]();
  
  registerSave(Saver.default);
  registerLoad(Loader.default);
  
  /** Registers the given function for saving objects of the given type. */
  def registerSave[VV](saveFn : ((File,VV) => RA => Unit))(implicit valType : Manifest[VV]) {
    val saver = new Saver {
      override def canSave[V](implicit localValType : scala.reflect.Manifest[V]) =
        valType.toString == localValType.toString;
      
      override def save[V](file : java.io.File, value : V)(implicit valType : scala.reflect.Manifest[V], ra : RA) =
        saveFn(file, value.asInstanceOf[VV])(ra);
    }
    
    registerSave(saver);
  }
  
  /** Registers the given Saver for saving objects of the given type. */
  def registerSave[V](saver : Saver)(implicit valType : Manifest[V]) {
    if (!savers.contains(saver)) {
      savers.insert(0, saver);
    }
  }
  
  /** Registers the given function for loading objects of the given type. */
  def registerLoad[VV](loadFn : (File => RA => VV))(implicit valType : Manifest[VV]) {
    val loader = new Loader {
      override def canLoad[V](implicit localValType : scala.reflect.Manifest[V]) =
        valType.toString == localValType.toString;
      
      override def load[V](file : java.io.File)(implicit valType : scala.reflect.Manifest[V], ra : RA) =
        loadFn(file)(ra).asInstanceOf[V];
    }
    
    registerLoad(loader);
  }
  
  /** Register the given Loader for loading objects of the given type. */
  def registerLoad[V](loader : Loader)(implicit valType : Manifest[V]) {
    if (!loaders.contains(loader)) {
      loaders.insert(0, loader);
    }
  }

  /** Make sure the class and object are loaded to run any static loader code */
  private def prepare[V](valType : scala.reflect.Manifest[V]) {
    try {
      Class.forName(valType.erasure.getName);
      Class.forName(valType.erasure.getName+"$");
    } catch { case _ => () }
  }
  
  def load[V](file : File)(implicit valType : scala.reflect.Manifest[V], ra : RA) : V = {
    prepare(valType);
    
    for (loader <- loaders; if loader.canLoad(valType)) {
      ra.log("RA.Serializer.load "+file+" ...");
      val rv = loader.load(file)(valType,ra);;
      ra.log("RA.Serializer.load done ");
      return rv;
    }
    
    throw new java.io.NotSerializableException("No serializer found to support type "+valType);
  }
  
  def save[V](file : File, value : V)(implicit valType : scala.reflect.Manifest[V], ra : RA) {
    prepare(valType);
    
    for (saver <- savers; if saver.canSave(valType)) {
      ra.log("RA.Serializer.save "+file+" ...");
      saver.save(file, value)(valType, ra);
      ra.log("RA.Serializer.save done");
      return;
    }
    
    throw new java.io.NotSerializableException("No serializer found for "+valType);
  }
  
  //
  // default load/save pairs
  //
  
  registerSave {
    (f : java.io.File, text : String) => (ra : RA) => {
      import ra.pipes._;
      Iterator.single(text) | f;
    }
  }
  
  registerSave {
    (f : java.io.File, iter : Iterator[String]) => (ra : RA) => {
      import ra.pipes._;
      iter | f;
    }
  }
  
  registerLoad[String] {
    (f : java.io.File) => (ra : RA) => {
      import ra.pipes._;
      f.getLines.mkString("\n");
    }
  }
  
  registerLoad[Iterator[String]] {
    (f : java.io.File) => (ra : RA) => {
      import ra.pipes._;
      f.getLines;
    }
  }
}
