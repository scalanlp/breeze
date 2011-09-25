package scalanlp.config

import com.thoughtworks.paranamer.AdaptiveParanamer
import java.lang.reflect.Type
import collection.mutable.ArrayBuffer
import java.{lang=>jl}
import java.io.File

/**
 *  Generates a Help message from a case class. If a constructor parameter has a "Help" annotation
 *  present, it will display that along with basic type information.
 * @author dlwh
 */
object GenerateHelp {
  def apply[C:Manifest]:String = {
    val man = implicitly[Manifest[C]]
    def recGen(clss: Class[_], prefix: String, buf: StringBuilder = new StringBuilder):StringBuilder = {
      val ann = clss.getAnnotation(classOf[Help])
      if(ann != null) {
        buf ++= "---\n"
        buf ++= ("Parameter Group " + prefix + " (" + clss.getName +")\n")
        buf ++= ann.text()
        buf += '\n'
        buf ++= "---\n"
      }

      // Handle ctor parameters
      val toRecurse = ArrayBuffer[(String,Class[_])]()

      val ctor = clss.getConstructors apply 0
      val reader = new AdaptiveParanamer();
      val paramNames = reader.lookupParameterNames(ctor);
      val anns = ctor.getParameterAnnotations
      for( ((name,tpe),myAnns) <- paramNames.zip(ctor.getGenericParameterTypes).zip(anns)) {
        val ann = myAnns.collectFirst{case h: Help => h}
        ann match {
          case None if isPrimitive(tpe) =>
            buf ++= "  " ++= wrap(prefix,name) ++= ": " ++= prettyString(tpe) ++= "\n"
          case Some(help) =>
            buf ++= "  " ++= wrap(prefix,name) ++= ": " ++= prettyString(tpe) ++= "  \t" ++= help.text() ++= "\n"
          case _ =>
        }
        if(!isPrimitive(tpe) && tpe.isInstanceOf[Class[_]]) {
          toRecurse += (name -> tpe.asInstanceOf[Class[_]])
        }
      }
      for( (name,tpe) <- toRecurse) {
        recGen(tpe,wrap(prefix,name),buf)
      }

      buf
    }

    recGen(man.erasure,"").toString
  }

  private def prettyString(tpe: Type) = tpe match {
    case jl.Integer.TYPE => "Int"
    case jl.Float.TYPE => "Float"
    case jl.Boolean.TYPE => "Boolean"
    case jl.Long.TYPE => "Long"
    case jl.Double.TYPE => "Double"
    case jl.Character.TYPE => "Char"
    case jl.Byte.TYPE => "Byte"
    case c: Class[_] => c.getName()
    case _ => tpe.toString
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
  case class Rec(i: Int)
  case class Params(str: Int, bo: Boolean, @Help(text="woooooo") f: File , rec: Rec)

  def main(args: Array[String]) {
    println(GenerateHelp[Params])
  }
}