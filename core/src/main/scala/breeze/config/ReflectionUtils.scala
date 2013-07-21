package breeze.config

import java.{lang=>jl}
import jl.reflect._

/**
 * Various utilities for determining properties of an object given Manifests and Class information
 * @author dlwh
 */
private[config] object ReflectionUtils {
  def manifestFromClass(c: Class[_]):Manifest[_] = {
    import java.lang._
    if(c == Float.TYPE) Manifest.Float
    else if(c == Long.TYPE) Manifest.Long
    else if(c == Double.TYPE) Manifest.Double
    else if(c == Integer.TYPE) Manifest.Int
    else if(c == Byte.TYPE) Manifest.Byte
    else if(c == Short.TYPE) Manifest.Short
    else if(c == Character.TYPE) Manifest.Char
    else if(c == Boolean.TYPE) Manifest.Boolean
    else if(c == Void.TYPE) Manifest.Unit
    else Manifest.classType(c)
  }

  /**
   * For a seq of parameters in a class's primary constructor, acquires the default parameters
   * for that class. The returned functions will throw exceptions if there
   * isn't a default for that parameter, or if there's an error processing it.
   *
   * Super hacky.
   */
  def lookupDefaultValues(clazz: Class[_], paramNames: Seq[String]): Seq[() => AnyRef] = {
    try {
      val companion = Class.forName(clazz.getName() + "$").getField("MODULE$").get(null)
      // defaults have the form  init$default$X, for X = 1...
      paramNames.zipWithIndex.map{
        case (name, idx) => () =>
          try {
            val method = companion.getClass.getMethod("init$default$" + (idx + 1))
            method.invoke(companion)
          } catch {
            case e: java.lang.NoSuchMethodException =>
              try {
                val method = companion.getClass.getMethod("$lessinit$greater$default$" + (idx + 1))
                method.invoke(companion)
              } catch {
                case e: java.lang.NoSuchMethodException =>
                throw new NoParameterException("Could not find a matching property!", name)
                case e: Exception =>
                  throw new RuntimeException("Problem processing default argument for " + name, e)
              }
            case e: Exception =>
              throw new RuntimeException("Problem processing default argument for " + name, e)
          }

      }
    } catch {
      case e: Exception =>
        paramNames.map{
          paramName => () => throw new NoParameterException("Could not find a matching property!", paramName)
        }
    }
  }

  /**
   * This is a relatively unsophisticated attempt to infer the types of type parameters for a dynamic
   * class given the static class.
   */
  def solveTypes(knownTypes: Map[String, OptManifest[_]], staticClass: Class[_], dynamicClass: Class[_]): Map[String, OptManifest[_]] = {
    if (!staticClass.isAssignableFrom(dynamicClass)) throw new RuntimeException(staticClass  + " is not assignable from " + dynamicClass + "!")
    // iterate up the inheritance chain
    def superTypes = if (dynamicClass.isInterface) (
      dynamicClass.getInterfaces.iterator
      )
    else /* class*/ (
      Iterator.iterate(dynamicClass.asInstanceOf[Class[AnyRef]])(_.getSuperclass.asInstanceOf[Class[AnyRef]])
        .takeWhile(clss => clss != null && staticClass.isAssignableFrom(clss))
      )
    val highestType = superTypes.reduceLeft((a, b) => b)
    val dynamicToStaticMapping: Map[String, OptManifest[_]] = superTypes.sliding(2, 1).foldRight(knownTypes) {
      (classPair, knownTypes) =>
        if (classPair.length < 2) knownTypes
        else {
          val generic = classPair(0).getGenericSuperclass
          extendMapping(classPair(0), classPair(1), generic, knownTypes)
        }
    }
    if (!staticClass.isInterface) {
      dynamicToStaticMapping
    } else {
      // solve for the interface too:
      val matchedIFace = highestType.getGenericInterfaces.find{
        case x: ParameterizedType =>
          x.getRawType == staticClass
        case x: Class[_] => x == staticClass
        case _ => false
      } getOrElse (highestType)
      extendMapping(highestType, staticClass, matchedIFace, dynamicToStaticMapping)
    }
  }

  /**
   * For the above method, tries to map the figure out the immediate subtype's parameters given the parent's
   */
  private def extendMapping(subType: Class[_], superType: Class[_], genericSuper: Type, knownTypes: Map[String, OptManifest[_]]) = {
    val mapping: Map[String, String] = genericSuper match {
      case x: ParameterizedType =>
        matchImmediateSubclassToSuperClass(subType, x, superType)
      case _ => Map.empty
    }
    mapping.mapValues(knownTypes).toMap withDefaultValue NoManifest

  }

  /*
   * returns a map from subclass's type parameters to the super type's.
   * Far from perfect, but works for simple cases, where there's an obvious match (no embedding in other types, and the like)
   * TODO: more sophisticated mapping, e.g. if we have Foo[A] extends Bar[List[A]], and class Bar[X], with X=List[Int]. Need to
   * infer A = Int
   */
  private def matchImmediateSubclassToSuperClass(sub: Class[_], genSup: ParameterizedType, sup: Class[_]): Map[String, String] = {
    val superTypeParams = sup.getTypeParameters.map(_.toString)
    genSup.getActualTypeArguments.map(_.toString) zip superTypeParams toMap
  }


  /**
   *
   *Tries to create the appropriate manifest for a jl.reflect.Type that has
   * using typeMap as a way of handling type parameters.
   */
  def mkManifest(typeMap: Map[String, OptManifest[_]], tpe: Type): Manifest[Object] = tpe match {
    case jl.Integer.TYPE => Manifest.Int.asInstanceOf[Manifest[Object]]
    case jl.Double.TYPE => Manifest.Double.asInstanceOf[Manifest[Object]]
    case jl.Float.TYPE => Manifest.Float.asInstanceOf[Manifest[Object]]
    case jl.Boolean.TYPE => Manifest.Boolean.asInstanceOf[Manifest[Object]]
    case tpe: Class[_] with ParameterizedType => new Manifest[Object] {
      override def runtimeClass: Class[_] = tpe
      override def typeArguments = tpe.getActualTypeArguments.map(mkManifest(typeMap, _)).toList

      override def toString = runtimeClass.getName + argString
    }
    case tpe: ParameterizedType =>
      val innerMan = mkManifest(Map.empty, tpe.getRawType)
      new Manifest[Object] {
        def runtimeClass = innerMan.runtimeClass

        override def typeArguments = tpe.getActualTypeArguments.map(mkManifest(typeMap, _)).toList

        override def toString = runtimeClass.getName + argString
      }
    case tpe: Class[_] => new Manifest[Object] {
      def runtimeClass = tpe

      override def toString = runtimeClass.getName + argString
    }
    case tpe: TypeVariable[_] => typeMap(tpe.toString) match {
      case x: Manifest[_] => x.asInstanceOf[Manifest[Object]]
      case _ => throw new CannotParseException("", "Don't know how to deal with " + tpe + " yet! Add an ArgumentParser.")
    }
    case tpe: GenericArrayType => mkManifest(typeMap, tpe.getGenericComponentType).arrayManifest.asInstanceOf[Manifest[Object]]
    case _ => throw new CannotParseException("", "Don't know how to deal with " + tpe + " yet! Add an ArgumentParser." + tpe.getClass.getName)
  }
}