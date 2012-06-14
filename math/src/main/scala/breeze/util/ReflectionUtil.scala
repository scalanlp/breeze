package breeze.util

/**
 *
 * @author dlwh
 */

object ReflectionUtil {
  def boxedFromPrimitive(c: Class[_]): Class[_] = {
    import java.lang._
    require(c.isPrimitive, "can't get boxed representation of non-primitive type")
    if(c == Float.TYPE) classOf[java.lang.Float]
    else if(c == Long.TYPE) classOf[java.lang.Long]
    else if(c == Double.TYPE) classOf[java.lang.Double]
    else if(c == Integer.TYPE) classOf[java.lang.Integer]
    else if(c == Byte.TYPE) classOf[java.lang.Byte]
    else if(c == Short.TYPE) classOf[java.lang.Short]
    else if(c == Character.TYPE) classOf[java.lang.Character]
    else if(c == Boolean.TYPE) classOf[java.lang.Boolean]
    else if(c == Void.TYPE) classOf[java.lang.Void]
    else sys.error("Shouldn't be here...")
  }

}
