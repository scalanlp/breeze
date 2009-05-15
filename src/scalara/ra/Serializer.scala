package scalara.ra;

import java.io.File;

import scala.reflect.Manifest;

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
      
      override def load[V](file : java.io.File)(implicit valType : scala.reflect.Manifest[V], ra : RA) : V = {
        import ra.pipes._;
        ra.log("RA.Serializer: default load for "+valType);
        val ois = new java.io.ObjectInputStream(file);
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
      
      override def save[V](file : java.io.File, value : V)(implicit valType : scala.reflect.Manifest[V], ra : RA) {
        import ra.pipes._;
        ra.log("RA.Serializer: default save for "+valType);
        val oos = new java.io.ObjectOutputStream(file);
        try { oos.writeObject(value) } finally { oos.close }
      }
    }
  }
  
  
  /** Savers with default implementation that use ObjectOutputStream. */
  protected var savers = new scala.collection.mutable.ArrayBuffer[Saver]();
  
  /** Loaders with default implementation that use ObjectInputStream. */
  protected var loaders = new scala.collection.mutable.ArrayBuffer[Loader]();
  
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
    (file : java.io.File, text : String) => (ra : RA) => {
      import ra.pipes._;
      Iterator.single(text) | file;
    }
  }
  
  registerLoad[String] {
    (file : java.io.File) => (ra : RA) => {
      import ra.pipes._;
      file.getLines.mkString("\n");
    }
  }
}
