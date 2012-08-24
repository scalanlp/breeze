package breeze.config

import com.thoughtworks.paranamer.AdaptiveParanamer
import java.lang.reflect.Type
import collection.mutable.ArrayBuffer
import java.io.File
import java.{lang=>jl}

import ReflectionUtils._

/**
 *  Generates a Help message from a case class. If a constructor parameter has a "Help" annotation
 *  present, it will display that along with basic type information.
 * @author dlwh
 */
object GenerateHelp {
  /**
   * Generates help for the given manifest
   */
  def apply[C:Manifest](conf: Configuration = Configuration.empty):String = {
    val man = implicitly[Manifest[C]]
    val reader = new AdaptiveParanamer()

    def recGen(staticManifest: Manifest[_], prefix: String):Seq[Format] = {
      val clss = staticManifest.erasure
      val ann = clss.getAnnotation(classOf[Help])
      val res = new ArrayBuffer[Format]
      if(ann != null) {
        res += Break += Group(prefix,clss.getName,ann.text) += Break
      }

      val dynamicClass: Class[_] = conf.recursiveGetProperty(prefix).map{
        Class.forName(_)
      } getOrElse (clss)
      if (dynamicClass.getConstructors.isEmpty)
        return res

      val staticTypeVars: Seq[String] = staticManifest.erasure.getTypeParameters.map(_.toString)
      val staticTypeVals: Seq[OptManifest[_]] = staticManifest.typeArguments
      val staticTypeMap: Map[String, OptManifest[_]] = (staticTypeVars zip staticTypeVals).toMap withDefaultValue (NoManifest)

      val dynamicTypeMap = solveTypes(staticTypeMap, staticManifest.erasure, dynamicClass)

      // Handle ctor parameters
      val toRecurse = ArrayBuffer[(String,Manifest[_])]()

      val ctor = dynamicClass.getConstructors apply 0
      val paramNames = reader.lookupParameterNames(ctor)
      val defaults = lookupDefaultValues(dynamicClass, paramNames)
      val anns = ctor.getParameterAnnotations
      val typedParams = ctor.getGenericParameterTypes.map { mkManifest(dynamicTypeMap, _)}

      for( i <- 0 until paramNames.length) {
        val myAnns = anns(i)
        val tpe = typedParams(i)
        val name = paramNames(i)
        val default = try { defaults(i)().toString } catch { case e: Exception => ""}
        val ann = myAnns.collectFirst{case h: Help => h}
        ann match {
          case None if isPrimitive(tpe.erasure) =>
            res += Param(wrap(prefix,name), prettyString(tpe),default, "")
          case Some(help) =>
            res += Param(wrap(prefix,name), prettyString(tpe),default, help.text)
          case _ =>
        }
        if(!isPrimitive(tpe.erasure)) {
          toRecurse += (name -> tpe)
        }
      }
      for( (name,tpe) <- toRecurse) {
        res ++= recGen(tpe,wrap(prefix,name))
      }

      res
    }

    val formats = recGen(man,"")
    val paramSplit = formats.foldLeft(0)(_ max _.paramLength)
    val minLength = formats.foldLeft(0)(_ max _.minLineLength)
    val buf = new StringBuilder()
    formats.foldLeft(buf) { (b,s) =>
      b ++= s.mkString(paramSplit - s.paramLength, minLength)
      b += '\n'
      b
    }

    buf.toString
  }

  private trait Format {
    def paramLength: Int = 0
    def minLineLength: Int = 0
    def mkString(splitWidth:Int, lineLength: Int):String
  }

  private case class Param(name: String, typeString: String, default: String, helpString: String) extends Format {
    override def paramLength = name.length + typeString.length + default.length + {if(default.length == 0) 4 else 7}
    override def minLineLength: Int = paramLength + helpString.length + 1
    def mkString(splitWidth: Int, lineLength: Int) = {
      "--" + name + ": " + typeString + {if(default.length == 0) "" else  " = " + default} +  (" " * splitWidth) + "  " + helpString
    }
  }

  private case object Break extends Format {
    def mkString(splitWidth:Int, lineLength: Int) = "=" * lineLength.min(10)
  }
  private case class Group(name: String, className: String, helpString: String) extends Format {
    val string = "Parameter Group " + name + " (" + className +")\n"
    def mkString(splitWidth:Int, lineLength: Int) = string +"\n" + helpString
    override def minLineLength = string.length max helpString.length
  }



  private def prettyString(tpe: Manifest[_]) = tpe match {
    case Manifest.Int => "Int"
    case Manifest.Float => "Float"
    case Manifest.Boolean => "Boolean"
    case Manifest.Long => "Long"
    case Manifest.Double => "Double"
    case Manifest.Char => "Char"
    case Manifest.Byte => "Byte"
    case c => if(c.erasure == classOf[String]) "String" else if (c.erasure == classOf[File]) "File" else tpe.toString
  }

  private def wrap(prefix: String, name: String):String = {
    if(prefix.isEmpty) name
    else prefix + "." + name
  }

  private val STRING = classOf[String]
  private val FILE = classOf[java.io.File]

  private def isPrimitive(tpe: Type):Boolean = tpe match {
    case jl.Integer.TYPE => true
    case jl.Float.TYPE => true
    case jl.Boolean.TYPE => true
    case jl.Long.TYPE => true
    case jl.Double.TYPE => true
    case jl.Character.TYPE => true
    case jl.Byte.TYPE => true
    case STRING => true
    case FILE => true
    case _ => false
  }

  @Help(text="Recursion works!")
  case class Rec[T](i: T)
  case class Params(str: Int, bo: Boolean, @Help(text="woooooo") f: File , rec: Rec[Int])

  def main(args: Array[String]) {
    println(GenerateHelp[Params]())
  }
}