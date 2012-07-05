package breeze.util

/*
 Copyright 2012 David Hall

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
