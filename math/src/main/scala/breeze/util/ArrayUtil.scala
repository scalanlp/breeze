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

import java.util.Arrays
import scala.reflect.ClassTag
import scala.util.hashing.MurmurHash3
import spire.std.float
import spire.syntax.cfor._
import breeze.macros.expand

/**
 * Array operations on generic arrays, a little faster in general, I hope.
 * @author dlwh
 */

object ArrayUtil {

  def fill[V](a: Array[V], offset: Int, length: Int, v: V): Unit = {
    a match {
      case x: Array[Double] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Double])
      case x: Array[Int] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Int])
      case x: Array[Float] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Float])
      case x: Array[Long] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Long])
      case x: Array[Short] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Short])
      case x: Array[Char] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Char])
      case x: Array[Byte] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Byte])
      case x: Array[Boolean] => Arrays.fill(x, offset, offset + length, v.asInstanceOf[Boolean])
      case x: Array[_] => Arrays.fill(x.asInstanceOf[Array[AnyRef]], offset, offset + length, v.asInstanceOf[AnyRef])
      case _ => throw new RuntimeException("shouldn't be here!")
    }
  }

  def copyOf[V](a: Array[V], length: Int): Array[V] = {
    a match {
      case x: Array[Double] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Int] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Float] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Long] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Short] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Char] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Byte] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[Boolean] => Arrays.copyOf(x, length).asInstanceOf[Array[V]]
      case x: Array[_] => Arrays.copyOf(x.asInstanceOf[Array[AnyRef]], length).asInstanceOf[Array[V]]
      case _ => throw new RuntimeException("shouldn't be here!")
    }
  }

  def copyOfRange[V, VU>:V](a: Array[V], from: Int, to: Int): Array[V] = {
    a match {
      case x: Array[Double] => Arrays.copyOfRange(x, from, to).asInstanceOf[Array[V]]
      case x: Array[Int] => Arrays.copyOfRange(x, from, to).asInstanceOf[Array[V]]
      case x: Array[Float] => Arrays.copyOfRange(x, from, to).asInstanceOf[Array[V]]
      case x: Array[Long] => Arrays.copyOfRange(x, from, to).asInstanceOf[Array[V]]
      case x: Array[Short] => Arrays.copyOfRange(x, from, to).asInstanceOf[Array[V]]
      case x: Array[Char] => Arrays.copyOfRange(x, from, to).asInstanceOf[Array[V]]
      case x: Array[Byte] => Arrays.copyOfRange(x, from, to).asInstanceOf[Array[V]]
      case x: Array[Boolean] => Arrays.copyOfRange(x, from, to).asInstanceOf[Array[V]]
      case x: Array[_] => Arrays.copyOfRange(x.asInstanceOf[Array[AnyRef]], from, to).asInstanceOf[Array[V]]
      case _ => throw new RuntimeException("shouldn't be here!")
    }
  }

  def newArrayLike[V](a: Array[V], length: Int): Array[V] = {
    a match {
      case x: Array[Double] => new Array[Double](length).asInstanceOf[Array[V]]
      case x: Array[Int] => new Array[Int](length).asInstanceOf[Array[V]]
      case x: Array[Float] => new Array[Float](length).asInstanceOf[Array[V]]
      case x: Array[Long] => new Array[Long](length).asInstanceOf[Array[V]]
      case x: Array[Short] => new Array[Short](length).asInstanceOf[Array[V]]
      case x: Array[Char] => new Array[Char](length).asInstanceOf[Array[V]]
      case x: Array[Byte] => new Array[Byte](length).asInstanceOf[Array[V]]
      case x: Array[Boolean] => new Array[Boolean](length).asInstanceOf[Array[V]]
      case x: Array[_] =>
        implicit val man = ClassTag[V](x.getClass.getComponentType.asInstanceOf[Class[V]])
        new Array[V](length)
      case _ => throw new RuntimeException("shouldn't be here!")
    }
  }

  /**
   * For reasons that I cannot explain java.util.Arrays.equals(Array(0.0), Array(-0.0)) == false
   * This method fixes that for floats and doubles
   */
  def nonstupidEquals(a: Array[_], aoffset: Int, astride: Int, alength: Int,
                      b: Array[_], boffset: Int, bstride: Int, blength: Int): Boolean = {
    if (a.getClass != b.getClass || alength != blength) false
    else {
      val (y, floating) = a match {
        case x: Array[Double] => b.asInstanceOf[Array[Double]] -> true
        case x: Array[Float] => b.asInstanceOf[Array[Float]] -> true
        case _ => b -> false
      }
      if (floating) {
        var ai = aoffset
        var bi = boffset
        var i = 0
        while (i < alength) {
          if (a(ai) != y(bi)) return false
          ai += astride
          bi += bstride
          i += 1
        }
        true
      } else {
        equals(a, aoffset, astride, alength, b, boffset, bstride, blength)
      }
    }
  }

  def equals(a: Array[_], b: Array[_]): Boolean = {
    if (a.getClass != b.getClass) false
    else {
      a match {
        case x: Array[Double] => Arrays.equals(a.asInstanceOf[Array[Double]], b.asInstanceOf[Array[Double]])
        case x: Array[Int] => Arrays.equals(a.asInstanceOf[Array[Int]], b.asInstanceOf[Array[Int]])
        case x: Array[Float] => Arrays.equals(a.asInstanceOf[Array[Float]], b.asInstanceOf[Array[Float]])
        case x: Array[Boolean] => Arrays.equals(a.asInstanceOf[Array[Boolean]], b.asInstanceOf[Array[Boolean]])
        case x: Array[Long] => Arrays.equals(a.asInstanceOf[Array[Long]], b.asInstanceOf[Array[Long]])
        case x: Array[Short] => Arrays.equals(a.asInstanceOf[Array[Short]], b.asInstanceOf[Array[Short]])
        case x: Array[Char] => Arrays.equals(a.asInstanceOf[Array[Char]], b.asInstanceOf[Array[Char]])
        case x: Array[Byte] => Arrays.equals(a.asInstanceOf[Array[Byte]], b.asInstanceOf[Array[Byte]])
        case x: Array[_] => Arrays.equals(a.asInstanceOf[Array[AnyRef]], b.asInstanceOf[Array[AnyRef]])
        case _ => throw new RuntimeException("shouldn't be here!")
      }
    }
  }

  def equals(a: Array[_], aoffset: Int, astride: Int, alength: Int,
             b: Array[_], boffset: Int, bstride: Int, blength: Int): Boolean = {
    if (a.getClass != b.getClass || alength != blength) false
    else if (aoffset == 0 && astride == 1 && alength == a.length && boffset == 0 && bstride == 1 && blength == b.length) {
      ArrayUtil.equals(a, b)
    } else {
      val y = a match {
        case x: Array[Double] => b.asInstanceOf[Array[Double]]
        case x: Array[Int] => b.asInstanceOf[Array[Int]]
        case x: Array[Float] => b.asInstanceOf[Array[Float]]
        case x: Array[Long] => b.asInstanceOf[Array[Long]]
        case x: Array[Short] => b.asInstanceOf[Array[Short]]
        case x: Array[Char] => b.asInstanceOf[Array[Char]]
        case x: Array[Byte] => b.asInstanceOf[Array[Byte]]
        case x: Array[Boolean] => b.asInstanceOf[Array[Boolean]]
        case x: Array[_] => b.asInstanceOf[Array[AnyRef]]
        case _ => throw new RuntimeException("shouldn't be here!")
      }
      var ai = aoffset
      var bi = boffset
      var i = 0
      while (i < alength) {
        if (a(ai) != y(bi)) return false
        ai += astride
        bi += bstride
        i += 1
      }
      true
    }
  }

  def gallopSearch(objs: Array[Int], fromIndex: Int, toIndex: Int, toFind: Int): Int = {
    if(objs.length == 0) return ~0
    // if(toIndex - fromIndex <= 16) return linearSearch(objs, fromIndex, toIndex, toFind)
    var low = fromIndex
    var step = 1
    var high = fromIndex + step
    while (high < toIndex && objs(high) < toFind) {
      low = high
      step *= 2
      high = fromIndex + step
    }
    if (high < toIndex && objs(high) == toFind) high
    else Arrays.binarySearch(objs, low, math.min(high, toIndex), toFind)
  }

  def zeroSkippingHashCode[V](data: Array[V], offset: Int, stride: Int, length: Int): Int = {
    (data: Any) match {
      case x: Array[Double] => zeroSkippingHashCodeImpl_Double(x, offset, stride, length)
      case x: Array[Float] => zeroSkippingHashCodeImpl_Float(x, offset, stride, length)
      case x: Array[Int] => zeroSkippingHashCodeImpl_Int(x, offset, stride, length)
      case x: Array[Long] => zeroSkippingHashCodeImpl_Long(x, offset, stride, length)
      case x: Array[Short] => zeroSkippingHashCodeImpl_Short(x, offset, stride, length)
      case x: Array[Byte] => zeroSkippingHashCodeImpl_Byte(x, offset, stride, length)
      case x: Array[Char] => zeroSkippingHashCodeImpl_Char(x, offset, stride, length)
      case x: Array[Boolean] => zeroSkippingHashCodeImpl_Boolean(x, offset, stride, length)
      case _ => zeroSkippingHashCodeImplSlow(data, offset, stride, length)
    }
  }

  @expand
  private def zeroSkippingHashCodeImpl[@expand.args(Int, Float, Double, Long, Byte, Short, Char, Boolean) V](data: Array[V], offset: Int, stride: Int, length: Int): Int = {
    var hash = 43
    var i = offset
    cforRange(0 until length) { _ =>
      val v = data(i)
      val hh = v.##
      if (hh != 0) hash = MurmurHash3.mix(hash, hh)
      i += stride
    }
    hash
  }

  private def zeroSkippingHashCodeImplSlow[V](data: Array[V], offset: Int, stride: Int, length: Int): Int = {
    var hash = 43
    var i = offset
    cforRange(0 until length) { _ =>
      val v = data(i)
      val hh = v.##
      if (hh != 0) hash = MurmurHash3.mix(hash, hh)
      i += stride
    }
    hash
  }

}
