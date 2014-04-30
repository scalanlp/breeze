package breeze.storage
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
import java.util.Arrays
import scala.reflect.ClassTag

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
trait ConfigurableDefault[@specialized V] extends Serializable { outer =>
  def value(implicit default: DefaultArrayValue[V]):V

  def fillArray(arr: Array[V], v: V) = arr.asInstanceOf[AnyRef] match {
    case x: Array[Int] => Arrays.fill(arr.asInstanceOf[Array[Int]], v.asInstanceOf[Int])
    case x: Array[Long] => Arrays.fill(arr.asInstanceOf[Array[Long]], v.asInstanceOf[Long])
    case x: Array[Short] => Arrays.fill(arr.asInstanceOf[Array[Short]], v.asInstanceOf[Short])
    case x: Array[Double] => Arrays.fill(arr.asInstanceOf[Array[Double]], v.asInstanceOf[Double])
    case x: Array[Float] => Arrays.fill(arr.asInstanceOf[Array[Float]], v.asInstanceOf[Float])
    case x: Array[Char] => Arrays.fill(arr.asInstanceOf[Array[Char]], v.asInstanceOf[Char])
    case x: Array[Byte] => Arrays.fill(arr.asInstanceOf[Array[Byte]], v.asInstanceOf[Byte])
    case x: Array[_] => Arrays.fill(arr.asInstanceOf[Array[AnyRef]], v.asInstanceOf[AnyRef])
    case _ => throw new RuntimeException("shouldn't be here!")
  }

  def makeArray(size:Int)(implicit default: DefaultArrayValue[V], man: ClassTag[V]) = {
    val arr = new Array[V](size)
    fillArray(arr,value(default))
    arr
  }

  def map[U](f: V=>U)(implicit dav: DefaultArrayValue[V]) = new ConfigurableDefault[U] {
    def value(implicit default: DefaultArrayValue[U]) = f(outer.value(dav))
  }
}



trait LowPriorityConfigurableImplicits {
  implicit def default[V]: ConfigurableDefault[V] = {
    new ConfigurableDefault[V] {
      def value(implicit default: DefaultArrayValue[V]) = default.value
    }
  }

}

object ConfigurableDefault extends LowPriorityConfigurableImplicits {
  implicit def fromV[V](v: V):ConfigurableDefault[V] = {
    new ConfigurableDefault[V] {
      def value(implicit default: DefaultArrayValue[V]) = v
    }
  }
}
