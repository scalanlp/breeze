package scalara.stringify;

import scala.reflect.Manifest;

import scalara.ra.RA;

object ReflectionUtils {
  import java.io.File;
  
  /** Returns the companion object for the given valType. */
  def companion[V](implicit valType : Manifest[V]) : Option[Class[_]] = {
    try {
      Some(valType.erasure.getClassLoader.loadClass(valType.erasure.getName+"$"));
    } catch {
      case _ => None;
    }
  }
  
  /**
   * Lists the fully qualified names of classes contained in the given path
   * (either a folder or a jar).
   */
  def listClasses(path : File) : List[String] = {
    def recurseFolders(prefix : String, root : File) : List[String] = {
      assert(root.isDirectory);
      val lists = (
        for (file <- root.listFiles) yield {
          if (file.isDirectory) {
            recurseFolders(prefix+file.getName+".", file);
          } else if (file.getName.endsWith(".class")) {
            List(prefix+file.getName.substring(0, file.getName.length - ".class".length));
          } else {
            List[String]();
          }
        }
      ).toList;
      
      for (l <- lists; e <- l) yield e;
    }
    
    def listJar(jarPath : File) : List[String] = {
      assert(jarPath.isFile);
      try {
        val jar = new java.util.jar.JarFile(jarPath);
        val iter = new Iterator[String] {
          val en = jar.entries;
          override def hasNext = en.hasMoreElements;
          override def next = en.nextElement.getName;
        }
        ( for (entry <- iter; if (entry.endsWith(".class"))) yield
            entry.replaceAll("/",".").substring(0, entry.length - ".class".length)
        ).toList;
      } catch {
        case ioe : java.io.IOException => List[String]();
      }
    }
    
    if (path.isDirectory) {
      recurseFolders("", path);
    } else {
      listJar(path);
    }
  }
  
  /** The list of all accessible class names in the default "env.classpath" environment. */
  lazy val classNames : List[String] = (
    for (path <- System.getProperty("env.classpath").split(System.getProperty("path.separator"));
         fullName <- listClasses(new File(path)))
      yield fullName
    ).toList;
  
  /** Returns a set of classes with the given name that may be in different packages. */
  def getClassesForName(name : String) : Set[Class[_]] = {
    Set() ++ (
      for (full <- classNames; if full.endsWith(name);
           val preceding = full(full.length - name.length - 1);
           if preceding == '$' || preceding == '.')
        yield Class.forName(full)
    );
  }
}

case class Test(x : Int);

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
    
    /** Consume the prefix of in as an instance of V, returning remainder. */
    def partial[V](in : String)(implicit valType : Manifest[V]) : Option[(V,CharSequence)];
  
    /** Consume all of in as an instance of V, throwing an exception if there is a remainder. */
    def apply[V](in : String)(implicit valType : Manifest[V]) : V = {
      partial[V](in) match {
        case Some((value, remainder)) =>
          if (remainder.length > 0) throw new StringifyException("Unexpected remainder when parsing "+valType+":\n  "+in);
          value;
        case None =>
          throw new StringifyException("Could not parse as "+valType+":\n  "+in);
      }
    }
    
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
    import ReflectionUtils._;
    import java.lang.reflect.Method;

    /** Returns true if the given method is a FromString method. */
    def isFromString[V](method : Method)(implicit valType : Manifest[V]) : Boolean = {
      method.getName == "apply" &&
      method.getReturnType == valType.erasure &&
      method.getParameterTypes.forall(typ => hasFromString(Manifest.classType(typ)));
    }
    
    /**
     * A FromString for primitive data types.
     */
    val primitives = new FromString {
      
      private val prefix = "^\\s*";
      private val suffix = "(?=(,|\\s|$)).*";
      
      val constructors = Map[Class[_],(scala.util.matching.Regex, String => Any)](
        classOf[String]  -> ((prefix+"(\"([^\"\\\\]|\\\\.)*\")"+suffix).r,
                             (x : String) => x),
        
        classOf[Int]     -> ((prefix+"(-?(\\d+))"+suffix).r,
                             (x : String) => x.toInt),
        
        classOf[Long]    -> ((prefix+"(-?(\\d+))l"+suffix).r,
                             (x : String) => x.toLong),
        
        classOf[Short]   -> ((prefix+"(-?(\\d+))s"+suffix).r,
                             (x : String) => x.toShort),
        
        classOf[Boolean] -> ((prefix+"(t|true|f|false)"+suffix).r,
                             (x : String) => x.toLowerCase == "true" || x.toLowerCase == "t"),
                             
        classOf[Double]  -> ((prefix+"(NaN|nan|([+-]?(Inf|inf|((?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?))))"+suffix).r,
                             (x : String) => x.toLowerCase match {
                               case "nan"  => Double.NaN;
                               case "inf"  => Double.PositiveInfinity;
                               case "+inf" => Double.PositiveInfinity;
                               case "-inf" => Double.NegativeInfinity;
                               case _ => x.toDouble
                             }),
                             
        classOf[Float]   -> ((prefix+"((NaN|nan|([+-]?(Inf|inf|((?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:[eE][+-]?\\d+)?))))f)"+suffix).r,
                             (x : String) => x.substring(0,x.length-1).toLowerCase match {
                               case "nan"  => Float.NaN;
                               case "inf"  => Float.PositiveInfinity;
                               case "+inf" => Float.PositiveInfinity;
                               case "-inf" => Float.NegativeInfinity;
                               case _ => x.toFloat
                             })
      );
      
      override def isDefinedAt[V](implicit valType : Manifest[V]) : Boolean =
        constructors.contains(valType.erasure);
      
      override def partial[V](in : String)(implicit valType : Manifest[V]) : Option[(V,CharSequence)] = {
        val (pattern, constructor) = constructors(valType.erasure);
      
        pattern.findFirstMatchIn(in) match {
          case Some(m) => Some((constructor(m.group(1)).asInstanceOf[V], m.after(1)));
          case None => None;
        }
      }
      
      override def apply[V](in : String)(implicit valType : Manifest[V]) : V = {
        constructors(valType.erasure)._2(in).asInstanceOf[V];
      }
    }
    
    /**
     * A default FromString that uses reflection on the companion object.
     */
    val default = new FromString {
      override def isDefinedAt[V](implicit valType : Manifest[V]) : Boolean = {
        if (primitives.isDefinedAt[V])
          return true;
        
        for (c <- companion[V]; method <- c.getMethods; if isFromString[V](method))
          return true;
        
        return false;
      }
      
      override def partial[V](in : String)(implicit valType : Manifest[V]) : Option[(V,CharSequence)] = {
        if (primitives.isDefinedAt[V])
          return primitives.partial[V](in);
        
        val c = companion[V] match {
          case Some(cls) => cls;
          case None => throw new UnsupportedOperationException("No companion object for "+valType);
        }
        
        for (c <- companion[V]; method <- c.getMethods; if isFromString[V](method)) {
          // TODO this works with only a single arg, doesn't work off partial
          val arg = fromString(in)(Manifest.classType(method.getParameterTypes()(0))).asInstanceOf[Object];
          return Some(method.invoke(c.getField("MODULE$").get(), arg).asInstanceOf[V], "");
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
  
  def hasFromString[V](implicit valType : Manifest[V]) : Boolean = {
    getFromString[V] match {
      case Some(x) => true;
      case None => false;
    }
  }
  
  def getFromString[V](implicit valType : Manifest[V]) : Option[FromString] = {
    prepare(valType);
    for (fromString <- fromStrings; if fromString.isDefinedAt(valType)) {
      return Some(fromString);
    }
    return None;
  }
  
  def fromString[V](string : String)(implicit valType : Manifest[V]) : V = {
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
