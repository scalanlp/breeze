package breeze.util

import java.util.Arrays

/**
 *
 * @author dlwh
 */

object ArrayUtil {
  def copyOf[V](a: Array[V], length: Int): Array[V] = {
    a match {
      case x: Array[Int] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Long] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Short] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Double] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Float] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Char] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Byte] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[_] => Arrays.copyOf(x.asInstanceOf[Array[AnyRef]], length).asInstanceOf[Array[V]]
      case _ => throw new RuntimeException("shouldn't be here!")
    }
  }

  def equals(a: Array[_], b: Array[_]):Boolean = {
    val ac = a.getClass
    val bc = b.getClass
    if(ac != bc) false
    else {
      a match {
        case x: Array[Int] => Arrays.equals(a.asInstanceOf[Array[Int]], b.asInstanceOf[Array[Int]])
        case x: Array[Long] => Arrays.equals(a.asInstanceOf[Array[Long]], b.asInstanceOf[Array[Long]])
        case x: Array[Short] => Arrays.equals(a.asInstanceOf[Array[Short]], b.asInstanceOf[Array[Short]])
        case x: Array[Double] => Arrays.equals(a.asInstanceOf[Array[Double]], b.asInstanceOf[Array[Double]])
        case x: Array[Float] => Arrays.equals(a.asInstanceOf[Array[Float]], b.asInstanceOf[Array[Float]])
        case x: Array[Char] => Arrays.equals(a.asInstanceOf[Array[Char]], b.asInstanceOf[Array[Char]])
        case x: Array[Byte] => Arrays.equals(a.asInstanceOf[Array[Byte]], b.asInstanceOf[Array[Byte]])
        case x: Array[_] => Arrays.equals(a.asInstanceOf[Array[AnyRef]], b.asInstanceOf[Array[AnyRef]])
        case _ => throw new RuntimeException("shouldn't be here!")
      }
    }


  }

}
