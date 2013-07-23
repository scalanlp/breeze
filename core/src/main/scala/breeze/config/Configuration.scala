package breeze.config

/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import com.thoughtworks.paranamer.AdaptiveParanamer
import com.thoughtworks.paranamer.ParameterNamesNotFoundException
import java.io.File
import java.io.FileInputStream
import java.util.Properties
import scala.reflect.Manifest
import scala.reflect.OptManifest
import scala.reflect.NoManifest

import scala.collection.JavaConverters._

import ReflectionUtils._
import scala.collection.mutable
import collection.generic.GenericCompanion
import scala.util.Try
import Configuration._

/**
 * Configuration provides a way to read in parameters
 * from an underlying representation, usually
 * a properties file. It automatically parses primitives, strings,
 * and Files, and has the ability to handle more complex types
 * via reflection and byte code munging. For instance, we
 * can read in the following:
 *
 * <code>
 * case class Foo(bool: Boolean, bar: String, path: File, logLevel: Int = 3)
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

  def allPropertyNames:Set[String]

  /**
   * Read in a T using a prefix, backing off to default, if necessary.
   */
  final def readIn[T: Manifest](prefix: String, default: => T): T = {
    try readIn[T](prefix) catch {
      case (e: NoParameterException) => default
    }
  }

  /**
   * Read in an object, boxing it if necessary.
   */
  final def readOpt[T: Manifest](prefix: String="") = {
    try (Some(readIn[T](prefix))) catch {
      case (e: NoParameterException) => None
    }
  }

  /**
   * Reads in an object, but throw an exception if not found.
   */
  final def readIn[T: Manifest](prefix: String="", enforceFullCoverage: Boolean = false): T = {
    val (t, touched) = readInTouched(prefix)
    if(enforceFullCoverage) {
      val remaining = allPropertyNames -- touched
      if(remaining.nonEmpty) throw new UnusedOptionsException[T](prefix, remaining)
      else t
    } else {
      t
    }
  }

  /**
   * Reads in an object, and return the set of property keys we touched
   */
  final def readInTouched[T: Manifest](prefix: String): (T, Set[String]) = {
    ArgumentParser.getArgumentParser[T] match {
      case Some(parser) =>
         recursiveGetProperty(prefix) match {
           case Some((prop, touched)) => parser.parse(prop) -> Set(touched)
           case None => throw new NoParameterException("Could not find matching property for " + prefix, prefix)
         }
      case None =>
        val man = implicitly[Manifest[T]]
        if(isCollectionType(man))  {
          readSequence(prefix, man, containedType(man)).asInstanceOf[(T, Set[String])]
        } else {
          reflectiveReadIn[T](prefix)
        }
    }
  }

  /**
   * Read in a T using a prefix, backing off to default, if necessary.
   */
  final def readInTouched[T: Manifest](prefix: String, default: => T): (T, Set[String]) = {
    try readInTouched[T](prefix) catch {
      case (e: NoParameterException) => default -> Set.empty
    }
  }

  /** Use properties from that if not found in this */
  def backoff(that: Configuration):Configuration = new Configuration {
    def getProperty(property: String) = outer.getProperty(property).orElse(that.getProperty(property))

    val allPropertyNames: Set[String] = outer.allPropertyNames ++ that.allPropertyNames
  }

  /**
   * Determines whether or not this type is a collection type
   * @param man the manifest to examine
   * @return
   */
  private def isCollectionType(man: Manifest[_]) = {
    man.runtimeClass.isArray || classOf[Iterable[_]].isAssignableFrom(man.runtimeClass)
  }

  /**
   * Gets the contained type from a container
   * @param man the manifest to examine
   * @return
   */
  private def containedType(man: Manifest[_]): Manifest[_] = {
    if(man.runtimeClass.isArray) {
      val comp = man.runtimeClass.getComponentType
      ReflectionUtils.manifestFromClass(comp)
    }
    else man.typeArguments.head
  }

  /**
   * Reads in a sequence by looking for properties of the form prefix.0, prefix.1 etc
   */
  private def readSequence[T](prefix: String, container: Manifest[_], contained: Manifest[T]):(AnyRef, Set[String]) = {
    val builder = {
      if(container.runtimeClass.isArray)
        mutable.ArrayBuilder.make()(contained)
      else {
        try {
          // try to construct a builder by going through the companion
          container.runtimeClass.newInstance().asInstanceOf[Iterable[T]].companion.newBuilder[T]
        } catch {
          case e: Exception => // hope the companion is named like we want...
            try {
              Class.forName(container.runtimeClass.getName + "$").getField("MODULE$").get(null).asInstanceOf[GenericCompanion[Iterable]].newBuilder[T]
            } catch {
              case e: Exception =>
              throw new NoParameterException("Can't figure out what to do with a sequence of type:" + container, prefix)
            }
        }

      }
    }
    var ok = true
    var i = 0
    var touched = Set.empty[String]
    while(ok) {
      try {
        val (t, myTouched) = readInTouched(wrap(prefix,i.toString))(contained)
        builder += t
        touched ++= myTouched
      } catch {
        case e: NoParameterException => ok = false
      }
      i += 1
    }

     builder.result() -> touched
  }

  // We have a static type, and a dynamic type.
  // The dynamic type will have to be inferred.
  // Some attempts are made to deal with generics
  private def reflectiveReadIn[T: Manifest](prefix: String): (T, Set[String]) = {
    val staticManifest = implicitly[Manifest[T]]

    val (dynamicClass: Class[_], touchedProperties) = {
      recursiveGetProperty(prefix) match {
        case Some((prop, propName)) =>
          // replace each . in time with a $, for inner classes.
          Class.forName(prop) -> Set(propName)
        case None => staticManifest.runtimeClass -> Set.empty[String]
      }
    }
    if (dynamicClass.getConstructors.isEmpty)
      throw new NoParameterException("Could not find a constructor for type " + dynamicClass.getName, prefix)

    val staticTypeVars: Seq[String] = staticManifest.runtimeClass.getTypeParameters.map(_.toString)
    val staticTypeVals: Seq[OptManifest[_]] = staticManifest.typeArguments
    val staticTypeMap: Map[String, OptManifest[_]] = (staticTypeVars zip staticTypeVals).toMap withDefaultValue NoManifest

    val dynamicTypeMap = solveTypes(staticTypeMap, staticManifest.runtimeClass, dynamicClass)

    try {
      // pick a constructor and figure out what the parameters names are
      val ctor = dynamicClass.getConstructors.last
      val reader = new AdaptiveParanamer()
      val paramNames = reader.lookupParameterNames(ctor)
      // Also get their types
      val typedParams = ctor.getGenericParameterTypes.map( mkManifest(dynamicTypeMap, _) )
      // and defaults, where possible
      val defaults = lookupDefaultValues(dynamicClass, paramNames)
      val namedParams = for {((tpe, name), default) <- typedParams zip paramNames zip defaults} yield (tpe, name, default)
      val (paramValues, touched) = namedParams.map {
        case (man, name, default) => readInTouched[Object](wrap(prefix,name), default())(man)
      }.unzip
      ctor.newInstance(paramValues: _*).asInstanceOf[T] -> touched.foldLeft(touchedProperties)(_ ++ _)
    } catch {
      case e: ParameterNamesNotFoundException =>
        throw new ConfigurationException("Could not find parameter names for " + dynamicClass.getName + " (" + prefix + ")") {
          def param = prefix
        }
    }
  }

  // tries to read prefix of the form "some.value.etc" and then tries "value.etc", continuing on
  private[config] def recursiveGetProperty(prefix: String): Option[(String, String)] = {
    if (prefix.isEmpty) None
    else getProperty(prefix) match {
      case opt@Some(p) => Some(p -> prefix)
      case None =>
        var next = prefix.dropWhile('.' !=)
        if (!next.isEmpty) next = next.drop(1)
        if (next.isEmpty) None else recursiveGetProperty(next)
    }
  }

  override def toString = {
    allPropertyNames.iterator.map(k => "  " + k + " -> " + getProperty(k).get).mkString("Configuration (\n",",\n", "\n)")
  }

  def toPropertiesString = {
    allPropertyNames.iterator.map(k => k + " = " + getProperty(k).get).mkString("\n")
  }

  def toCommandLineString = {
    allPropertyNames.iterator.map(k => "--" + k + " " + getProperty(k).get).mkString(" ")
  }

}

/** The exception thrown in case something goes wrong in Configuration
 * @author dlwh
 */
abstract class ConfigurationException(msg: String) extends Exception(msg) {
  def param:String
}

/** The exception thrown in case something goes wrong in Configuration
  * @author dlwh
  */
class CannotParseException(param: String, msg: String) extends Exception(msg)

/**
 * The exception thrown for a missing property in Configuration
 */
class NoParameterException(msg: String, val param: String) extends ConfigurationException("while searching for " + param + ": " + msg)
class UnusedOptionsException[T:Manifest](val param: String, val unused: Set[String]) extends ConfigurationException(s"Some parameters were not read while parsing $param of type ${implicitly[Manifest[T]]}: $unused")

object Configuration {

  def empty: Configuration = fromMap(Map.empty)

  def apply(kv: (String,String)*): Configuration = fromMap(kv.toMap)

  /**
   * Creates a Configuration from a java.util.Properties.
   */
  def fromProperties(prop: Properties) = new Configuration {
    def getProperty(p: String) = {
      val v = prop.getProperty(p)
      if (v == null) None
      else Some(v)
    }

    def allPropertyNames: Set[String] = prop.stringPropertyNames().asScala.toSet -- System.getProperties.stringPropertyNames().asScala
  }

  /**
   * Creates a Configuration from a Map.
   */
  def fromMap(map: Map[String, String]):Configuration = new Configuration {
    def getProperty(p: String) = map.get(p)

    val allPropertyNames: Set[String] = map.keySet
  }


  /**
   * Creates a Configuration from a sequence of properties files, where later
   * properties files take precedence over early properties files.
   */
  def fromPropertiesFiles(args: Seq[File]) = {
    val props = args.foldLeft(System.getProperties) {
      (old, file) =>
        val props = new Properties(old)
        props.load(new FileInputStream(file))
        props
    }
    Configuration.fromProperties(props)
  }

  /**
   * extracts a Configuration object from a preexisting object. Mostly useful for saving information about how
   * an execution was configured.
   * @tparam T
   */
  def fromObject[T:Manifest](t: T, name: String =""):Configuration = {

    def rec[T:Manifest](t: T, name: String = ""):Map[String, String] = t match {
      case null => Map.empty[String, String]
      case _: Boolean => Map(name -> t.toString)
      case _: Int => Map(name -> t.toString)
      case _: Short => Map(name -> t.toString)
      case _: Long => Map(name -> t.toString)
      case _: Float => Map(name -> t.toString)
      case _: Double => Map(name -> t.toString)
      case _: Char => Map(name -> t.toString)
      case _: Byte => Map(name -> t.toString)
      case _: File => Map(name -> t.toString)
      case _: String => Map(name -> t.toString)
      case t: Class[_] => Map(name -> t.getName)
      case t: Array[_] => rec(t:IndexedSeq[_], name)
      case t: Iterable[_] =>  t.zipWithIndex.foldLeft(Map.empty[String,String]){ case (acc, (ti, i)) =>
        rec(ti.asInstanceOf[AnyRef], name+"." + i)(ReflectionUtils.manifestFromClass(ti.getClass).asInstanceOf[Manifest[AnyRef]])
      }
      case x:Product =>
        val staticManifest = implicitly[Manifest[T]]
        val dynamicClass = x.getClass
        var baseMap = if(dynamicClass != staticManifest.runtimeClass) {
          rec(dynamicClass, name)
        } else {
          Map.empty[String, String]
        }

        val staticTypeVars: Seq[String] = staticManifest.runtimeClass.getTypeParameters.map(_.toString)
        val staticTypeVals: Seq[OptManifest[_]] = staticManifest.typeArguments
        val staticTypeMap: Map[String, OptManifest[_]] = (staticTypeVars zip staticTypeVals).toMap withDefaultValue (NoManifest)
        val dynamicTypeMap = solveTypes(staticTypeMap, staticManifest.runtimeClass, dynamicClass)

        val ctor = dynamicClass.getConstructors.last
        val paramNames = reader.lookupParameterNames(ctor)
        val typedParams = ctor.getGenericParameterTypes.map { mkManifest(dynamicTypeMap, _)}
        val prefix = name
        for( i <- 0 until paramNames.length) {
          val tpe = typedParams(i)
          val name = paramNames(i)
          val meth = Try(x.getClass.getMethod(name)).getOrElse(x.getClass.getMethod(s"get${name.capitalize}"))
          if(!meth.isAccessible)
            meth.setAccessible(true)
          baseMap ++= rec(meth.invoke(x), wrap(prefix, name))(tpe.asInstanceOf[Manifest[AnyRef]])
        }

        baseMap


    }
    fromMap(rec(t, name))
  }

  private val reader = new AdaptiveParanamer()
  private def wrap(prefix: String, name: String):String = {
    if(prefix.isEmpty) name
    else prefix + "." + name
  }



}
