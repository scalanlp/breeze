package breeze.util

import java.util.Arrays

/**
 *
 * @author dlwh
 */

object ArrayUtil {
  def fill[V](a: Array[V], offset: Int, length: Int, v: V) {
    a match {
      case x: Array[Int] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Int])
      case x: Array[Long] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Long])
      case x: Array[Short] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Short])
      case x: Array[Double] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Double])
      case x: Array[Float] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Float])
      case x: Array[Char] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Char])
      case x: Array[Byte] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Byte])
      case x: Array[_] => Arrays.fill(x.asInstanceOf[Array[AnyRef]], offset, offset + length, v.asInstanceOf[AnyRef])
      case _ => throw new RuntimeException("shouldn't be here!")
    }
  }

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
