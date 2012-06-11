package scalanlp.config

/*
 Copyright 2010 David Hall, Daniel Ramage

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


import com.thoughtworks.paranamer.AdaptiveParanamer;
import com.thoughtworks.paranamer.ParameterNamesNotFoundException
import java.io.File
import java.io.FileInputStream
import java.util.Properties
import scala.reflect.Manifest;
import scala.reflect.OptManifest;
import scala.reflect.NoManifest;

import ReflectionUtils._

/**
 * Configuration provides a way to read in parameters
 * from an underlying representation, usually
 * a properties file. It automatically parses primitives, strings,
 * and Files, and has the ability to handle more complex types
 * via reflection and byte code munging. For instance, we
 * can read in the following:
 *
 * <code>
 * case class Foo(bool: Boolean, bar: String, path: File, logLevel: Int = 3);
 * </code>
 *
 * with the following command:
 * <code>
 *   config.readIn[Foo]("foo")
 * </code>
 * and properties file:
 * <code>
 *   foo.bool = true
 *   bar = test
 *   foo.path = /home/dlwh/
 * </code>
 *
 * logLevel will automatically be inferred from its default. Nested
 * classes are also supported. By default, Configuration will
 * search for a property using a layered approach: foo.baz.bar, then
 * baz.bar, then bar, following by a default if it's provided.
 *
 * Some amount of type inference of type arguments is performed, but it's
 * by no means perfect. If something doesn't work that you'd like to work,
 * ask on the mailing list.
 *
 * Subclassing is also supported. If you readIn[T]("foo"), and the
 * property foo is the name of a subclass of T (fully qualified), then
 * the subclass will be read instead.
 *
 * @author dlwh
 *
 */
trait Configuration { outer =>
  /**
   * Get a raw property without any processing. Returns None if it's not present.
   */
  def getProperty(property: String): Option[String]

  /**
   * Read in a T using a prefix, backing off to default, if necessary.
   */
  final def readIn[T: Manifest](prefix: String, default: => T): T = {
    try (readIn[T](prefix)) catch {
      case (e: NoParameterException) => default
    };
  }


  /**
   * Read in an object, boxing it if necessary.
   */
  final def readOpt[T: Manifest](prefix: String="") = {
    try (Some(readIn[T](prefix))) catch {
      case (e: NoParameterException) => None
    };
  }

  /**
   * Reads in an object, but throw an exception if not found.
   */
  final def readIn[T: Manifest](prefix: String=""): T = {
    ArgumentParser.getArgumentParser[T] match {
      case Some(parser) =>
        val property = recursiveGetProperty(prefix);
        if (property.isEmpty)
          throw new NoParameterException("Could not find matching property for " + prefix, prefix);
        parser.parse(property.get);
      case None => reflectiveReadIn[T](prefix);
    }
  }

  // We have a static type, and a dynamic type.
  // The dynamic type will have to be inferred.
  // Some attempts are made to deal with generics
  private def reflectiveReadIn[T: Manifest](prefix: String): T = {
    val staticManifest = implicitly[Manifest[T]];
    val dynamicClass: Class[_] = recursiveGetProperty(prefix).map{
      Class.forName(_)
    } getOrElse (staticManifest.erasure);
    if (dynamicClass.getConstructors.isEmpty)
      throw new NoParameterException("Could not find a constructor for type " + dynamicClass.getName, prefix);

    val staticTypeVars: Seq[String] = staticManifest.erasure.getTypeParameters.map(_.toString);
    val staticTypeVals: Seq[OptManifest[_]] = staticManifest.typeArguments;
    val staticTypeMap: Map[String, OptManifest[_]] = (staticTypeVars zip staticTypeVals).toMap withDefaultValue (NoManifest);

    val dynamicTypeMap = solveTypes(staticTypeMap, staticManifest.erasure, dynamicClass);

    try {
      val ctor = dynamicClass.getConstructors.last
      val reader = new AdaptiveParanamer();
      val paramNames = reader.lookupParameterNames(ctor);
      val typedParams = ctor.getGenericParameterTypes.map{
        mkManifest(dynamicTypeMap, _)
      }
      val defaults = lookupDefaultValues(dynamicClass, paramNames);
      val namedParams = for {((tpe, name), default) <- typedParams zip paramNames zip defaults} yield (tpe, name, default);
      val paramValues = namedParams.map{
        case (man, name, default) => readIn[Object](prefix + "." + name, default())(man)
      }
      println(dynamicClass)
      println(paramValues.mkString(", "))
      println(ctor)
//      println(paramValues.map(_.getClass).mkString(", "))
      ctor.newInstance(paramValues: _*).asInstanceOf[T];
    } catch {
      case e: ParameterNamesNotFoundException =>
        throw new ConfigurationException("Could not find parameter names for " + dynamicClass.getName + " (" + prefix + ")");
    }
  }

  private[config] def recursiveGetProperty(prefix: String): Option[String] = {
    if (prefix.isEmpty) None
    else getProperty(prefix) match {
      case opt@Some(p) => opt
      case None =>
        var next = prefix.dropWhile('.' !=);
        if (!next.isEmpty) next = next.drop(1);
        if (next.isEmpty) None else recursiveGetProperty(next);
    }
  }

  /** Use properties from that if not found in this */
  def backoff(that: Configuration):Configuration = new Configuration {
    def getProperty(property: String) = outer.getProperty(property).orElse(that.getProperty(property))
  }
}

/** The exception thrown in case something goes wrong in Configuration
 * @author dlwh
 */
class ConfigurationException(msg: String) extends Exception(msg);

/**
 * The exception thrown for a missing property in Configuration
 */
class NoParameterException(msg: String, param: String) extends ConfigurationException("while searching for " + param + ": " + msg);

object Configuration {

  def empty = fromMap(Map.empty)

  /**
   * Creates a Configuration from a java.util.Properties.
   */
  def fromProperties(prop: Properties) = new Configuration {
    def getProperty(p: String) = {
      val v = prop.getProperty(p);
      if (v == null) None
      else Some(v);
    }
  }

  /**
   * Creates a Configuration from a Map.
   */
  def fromMap(map: Map[String, String]):Configuration = new Configuration {
    def getProperty(p: String) = map.get(p);
  }


  /**
   * Creates a Configuration from a sequence of properties files, where later
   * properties files take precedence over early properties files.
   */
  def fromPropertiesFiles(args: Seq[File]) = {
    val props = args.foldLeft(System.getProperties) {
      (old, file) =>
        val props = new Properties(old);
        props.load(new FileInputStream(file));
        props
    }
    Configuration.fromProperties(props);
  }

}
