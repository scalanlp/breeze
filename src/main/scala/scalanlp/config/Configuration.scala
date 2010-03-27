package scalanlp.config
import com.thoughtworks.paranamer.AdaptiveParanamer;
import com.thoughtworks.paranamer.ParameterNamesNotFoundException
import java.lang.reflect.Type
import java.lang.reflect.ParameterizedType;
import java.{lang=>jl}
import java.lang.reflect.TypeVariable
import java.util.Properties
import scala.reflect.Manifest;
import scala.reflect.OptManifest;
import scala.reflect.NoManifest;

trait Configuration {
  def getProperty(property:String):Option[String]

  final def readIn[T:Manifest](prefix:String, default : =>T):T = {
    try(readIn[T](prefix)) catch { case (e:NoParameterException) => default };
  }

  final def readIn[T:Manifest](prefix:String):T = {
    ArgumentParser.getArgumentParser[T] match {
      case Some(parser) =>
        val property = recursiveGetProperty(prefix);
        if(property.isEmpty)
          throw new NoParameterException("Could not find matching property for " + prefix,prefix);
        parser.parse(property.get);
      case None => reflectiveReadIn[T](prefix);
    }
  }

  private def reflectiveReadIn[T:Manifest](prefix: String):T = {
    val staticManifest = implicitly[Manifest[T]];
    val dynamicClass:Class[_] = recursiveGetProperty(prefix).map {
      Class.forName(_)
    } getOrElse (staticManifest.erasure);

    val staticTypeVars:Seq[String] = staticManifest.erasure.getTypeParameters.map(_.toString);
    val staticTypeVals:Seq[OptManifest[_]] = staticManifest.typeArguments;
    val staticTypeMap : Map[String,OptManifest[_]] = (staticTypeVars zip staticTypeVals).toMap withDefaultValue (NoManifest);

    val dynamicTypeMap = solveTypes(staticTypeMap,staticManifest.erasure,dynamicClass);

    try {
      val ctor = dynamicClass.getConstructors.apply(0);
      val reader = new AdaptiveParanamer();
      val paramNames = reader.lookupParameterNames(ctor);
      val typedParams = ctor.getGenericParameterTypes.map{ mkManifest(dynamicTypeMap, _)  }
      val namedParams = typedParams.zip(paramNames);
      val paramValues = namedParams.map { case (man,name) => readIn[Object](prefix + "." + name)(man) }
      ctor.newInstance(paramValues:_*).asInstanceOf[T];
    } catch {
      case e: ParameterNamesNotFoundException =>
        throw new ConfigurationException("Could not find parameter names for "+ dynamicClass.getName + " ("+prefix + ")");
    }
  }

  private def solveTypes(knownTypes: Map[String,OptManifest[_]], staticClass: Class[_], dynamicClass: Class[_]):Map[String,OptManifest[_]] = {
    val dynamicParams = dynamicClass.getTypeParameters;
    val dynamicToStaticMapping: Map[String,OptManifest[_]] = if(staticClass.isInterface) {
      val matchedIFace = dynamicClass.getGenericInterfaces.find { case x : ParameterizedType =>
          x.getRawType == staticClass
        case x:Class[_] => x == staticClass
        case _ => false
      } get;
      matchedIFace match {
        case x : ParameterizedType =>
          val mapping:Map[String,String] = matchImmediateSubclassToSuperClass(dynamicClass,x,x.getRawType.asInstanceOf[Class[_]]);
          mapping.mapValues(knownTypes).toMap withDefaultValue NoManifest;
        case _ => Map.empty withDefaultValue NoManifest;
      }
    } else {
      // iterate up the inheritance chain
      val superTypes = Iterator.iterate(dynamicClass.asInstanceOf[Class[AnyRef]])(_.getSuperclass.asInstanceOf[Class[AnyRef]]);
      // i.e. take all types up to and including staticClass, then map the types
      superTypes.takeWhile{staticClass.isAssignableFrom(_)}.sliding(2,1).foldRight(knownTypes) { (classPair,knownTypes) =>
        if(classPair.length < 2) knownTypes
        else {
          val generic = classPair(0).getGenericSuperclass;
          val mapping:Map[String,String] = generic match {
            case x: ParameterizedType =>
              matchImmediateSubclassToSuperClass(classPair(0),x,classPair(1));
            case _ => Map.empty;
          }
          mapping.mapValues(knownTypes).toMap withDefaultValue NoManifest;
        }
      }
    }
    dynamicToStaticMapping;
  }

  /**
   * returns a map from subclass's type parameters to the super type's.
   * Far from perfect, but works for simple cases, where there's an obvious match (no embedding in other types, and the like)
   * TODO: more sophisticated mapping, e.g. if we have Foo[A] extends Bar[List[A]], and class Bar[X], with X=List[Int]. Need to
   * infer A = Int
   */
  private def matchImmediateSubclassToSuperClass(sub: Class[_], genSup: ParameterizedType, sup: Class[_]):Map[String,String] = {
    val superTypeParams = sup.getTypeParameters.map(_.toString);
    genSup.getActualTypeArguments.map(_.toString) zip superTypeParams toMap;
  }


  private def mkManifest(typeMap: Map[String,OptManifest[_]], tpe: Type):Manifest[Object] = tpe match {
    case jl.Integer.TYPE => Manifest.Int.asInstanceOf[Manifest[Object]];
    case jl.Double.TYPE => Manifest.Double.asInstanceOf[Manifest[Object]];
    case jl.Float.TYPE => Manifest.Float.asInstanceOf[Manifest[Object]];
    case jl.Boolean.TYPE => Manifest.Boolean.asInstanceOf[Manifest[Object]];
    case tpe:Class[_] with ParameterizedType => new Manifest[Object] {
      def erasure = tpe;
      override def typeArguments = tpe.getActualTypeArguments.map(mkManifest(typeMap,_)).toList;
      override def toString = erasure.getName + argString;
    }
    case tpe:Class[_] => new Manifest[Object] {
      def erasure = tpe;
      override def toString = erasure.getName + argString;
    }
    case tpe:TypeVariable[_] => typeMap(tpe.toString) match {
      case x: Manifest[_] => x.asInstanceOf[Manifest[Object]];
      case _ => throw new ConfigurationException("Don't know how to deal with " + tpe + " yet! Add an ArgumentParser.");
    }
    case _ => throw new ConfigurationException("Don't know how to deal with " + tpe + " yet! Add an ArgumentParser.");
  }

  private def recursiveGetProperty(prefix: String):Option[String] = getProperty(prefix) match {
    case opt@Some(p) => opt
    case None =>
      var next = prefix.dropWhile('.'!=);
      if(!next.isEmpty) next = next.drop(1);
      if(next.isEmpty) None else recursiveGetProperty(next);
  }
}

class ConfigurationException(msg:String) extends Exception(msg);
class NoParameterException(msg: String,param: String) extends ConfigurationException(msg);

object Configuration {
  def fromProperties(prop: Properties) = new Configuration {
    def getProperty(p: String) = {
      val v = prop.getProperty(p);
      if(v == null) None
      else Some(v);
    }
  }
}