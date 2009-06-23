package scalara.stringify;

import scala.reflect.Manifest;

import scalara.ra.RA;

/**
 * Custom flexibly conversion to and from Strings.
 * 
 * @author dramage
 */
object Stringify {
  
  trait ToString {
    def isDefinedAt[V](implicit valType : Manifest[V]) : Boolean;
    def apply[V](in : V)(implicit valType : Manifest[V]) : String;
  }
  
  trait FromString {
    def isDefinedAt[V](implicit valType : Manifest[V]) : Boolean;
    def apply[V](in : String)(implicit valType : Manifest[V]) : V;
  }
  
  object ToString {
    val default = new ToString {
      override def isDefinedAt[V](implicit valType : Manifest[V]) : Boolean =
        true;
      
      override def apply[V](in : V)(implicit valType : Manifest[V]) : String =
        in.toString;
    }
  }
  
  object FromString {
    val default = new FromString {
      override def isDefinedAt[V](implicit valType : Manifest[V]) : Boolean = {
        val companion = try {
          valType.erasure.getClassLoader.loadClass(valType.erasure.getName+"$");
        } catch {
          case _ =>
            return false;
        }
        
        for (method <- companion.getMethods;
             if method.getReturnType == valType.erasure;
             if method.getName == "apply";
             if method.getParameterTypes.toList == List(classOf[String])) {
          
          return true;
        }
        
        false;
      }
      
      override def apply[V](in : String)(implicit valType : Manifest[V]) : V = {
        val companion = try {
          valType.erasure.getClassLoader.loadClass(valType.erasure.getName+"$");
        } catch {
          case _ => throw new UnsupportedOperationException("No companion object for "+valType);
        }
        
        for (method <- companion.getMethods;
             if method.getReturnType == valType.erasure;
             if method.getName == "apply";
             if method.getParameterTypes.toList == List(classOf[String])) {
          
          return method.invoke(companion.getField("MODULE$").get(), in).asInstanceOf[V];
        }
        
        throw new UnsupportedOperationException("No string argument constructor found");
      }
    }
  }
  
  /** Savers with default implementation that use ObjectOutputStream. */
  protected val toStrings = new scala.collection.mutable.ArrayBuffer[ToString]();
  
  /** Loaders with default implementation that use ObjectInputStream. */
  protected val fromStrings = new scala.collection.mutable.ArrayBuffer[FromString]();
  
  registerToString(ToString.default);
  registerFromString(FromString.default);
  
  def registerToString(s : ToString) =
    toStrings += s;
  
  def registerFromString(s : FromString) =
    fromStrings += s;
  
  /** Make sure the class and object are loaded to run any static loader code */
  private def prepare[V](valType : scala.reflect.Manifest[V]) {
    try {
      Class.forName(valType.erasure.getName);
      Class.forName(valType.erasure.getName+"$");
    } catch { case _ => () }
  }
  
  def getFromString[V](implicit valType : Manifest[V]) : Option[FromString] = {
    prepare(valType);
    for (fromString <- fromStrings; if fromString.isDefinedAt(valType)) {
      return Some(fromString);
    }
    return None;
  }
  
  def fromString[V](string : String)(implicit valType : Manifest[V], ra : RA) : V = {
    getFromString[V] match {
      case Some(f) =>
        f[V](string);
      case _ =>
        throw new StringifyException("No FromString found for "+valType);
    }
  }
  
  def getToString[V](implicit valType : Manifest[V]) : Option[ToString] = {
    prepare(valType);
    for (toString <- toStrings; if toString.isDefinedAt(valType)) {
      return Some(toString);
    }
    None;
  }
  
  def toString[V](value : V)(implicit valType : Manifest[V], ra : RA) {
    getToString[V] match {
      case Some(f) =>
        f[V](value);
      case _ =>
        throw new StringifyException("No ToString found for "+valType);
    }
  }
  
}

class StringifyException(msg : String) extends RuntimeException(msg);
