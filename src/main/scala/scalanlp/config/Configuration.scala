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

trait Configuration {
  def getProperty(property:String):Option[String]

  final def readIn[T:Manifest](prefix:String):T = {
    ArgumentParser.getArgumentParser[T] match {
      case Some(parser) =>
        val property = recursiveGetProperty(prefix);
        if(property.isEmpty)
          throw new ConfigurationException("Could not find matching property for " + prefix);
        parser.parse(property.get);
      case None => reflectiveReadIn[T](prefix);
    }
  }

  private def reflectiveReadIn[T:Manifest](prefix: String):T = {
    val man = implicitly[Manifest[T]];
    val reader = new AdaptiveParanamer();
    val typeVars:Seq[String] = man.erasure.getTypeParameters.map(_.toString);
    val typeVals:Seq[OptManifest[_]] = man.typeArguments;
    val typeMap : Map[String,OptManifest[_]] = typeVars zip typeVals toMap;
    try {
      val ctor = man.erasure.getConstructors.apply(0);
      val paramNames = reader.lookupParameterNames(ctor);
      val typedParams = ctor.getGenericParameterTypes.map{ mkManifest(typeMap, _)  }
      val namedParams = typedParams.zip(paramNames);
      val paramValues = namedParams.map { case (man,name) => readIn[Object](prefix + "." + name)(man) }
      ctor.newInstance(paramValues:_*).asInstanceOf[T];
    } catch {
      case e: ParameterNamesNotFoundException =>
        throw new ConfigurationException("Could not find parameters for "+ man + " ("+prefix + ")");
    }
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

object Configuration {
  def fromProperties(prop: Properties) = new Configuration {
    def getProperty(p: String) = {
      val v = prop.getProperty(p);
      if(v == null) None
      else Some(v);
    }
  }
}