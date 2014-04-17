package breeze

import java.util.zip._
import java.io._
import scala.collection.generic._
import scala.collection.mutable
import java.util.BitSet

/**
 *
 * @author dlwh
 */
package object util {

  /**
   * Deserializes an object using java serialization
   */
  def readObject[T](loc: File) = {
    val stream = new BufferedInputStream(new GZIPInputStream(new FileInputStream(loc)))
    val oin = nonstupidObjectInputStream(stream)
    val result = oin.readObject().asInstanceOf[T]
    oin.close()
    result
  }

  /**
   * For reasons that are best described as asinine, ObjectInputStream does not take into account
   * Thread.currentThread.getContextClassLoader. This fixes that.
   *
   * @param stream
   * @return
   */
  def nonstupidObjectInputStream(stream: InputStream):ObjectInputStream =  {
    new ObjectInputStream(stream) {
      @throws[IOException]
      @throws[ClassNotFoundException]
      override def resolveClass(desc: ObjectStreamClass): Class[_] = {
        try {
          val currentTccl: ClassLoader = Thread.currentThread.getContextClassLoader
          currentTccl.loadClass(desc.getName)
        } catch {
          case e: Exception =>
            super.resolveClass(desc)
        }
      }
    }
  }



  /**
   * Serializes an object using java serialization
   */
  def writeObject[T](out: File, parser: T): Unit = {
    val stream = new ObjectOutputStream(new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream(out))))
    stream.writeObject(parser)
    stream.close()
  }

  /**
   * You can write TODO in your code, and get an exception at runtime for any expression.
   */
  def TODO = sys.error("TODO (Not implemented)")

  /**
   * You can write XXX in your code and get an exception at runtime for any expression.
   */
  def XXX = sys.error("XXX Not Implemented")
  /**
   * Similar to the TODO expression, except this one is for types.
   */
  type TODO = Nothing

  /**
   * Computes the current source file and line number.
   */
  @noinline def LOCATION = {
    val e = new Exception().getStackTrace()(1)
    e.getFileName() + ":" + e.getLineNumber()
  }

  /**
   * Computes the source file location of the nth parent.
   * 0 is equivalent to LOCATION
   */
  @noinline def CALLER(nth : Int) = {
    val e = new Exception().getStackTrace()(nth+1)
    e.getFileName() + ":" + e.getLineNumber()
  }

  /**
   * Returns a string with info about the available and used space.
   */
  def memoryString = {
    val r = Runtime.getRuntime
    val free = r.freeMemory / (1024 * 1024)
    val total = r.totalMemory / (1024 * 1024)
    ((total - free) + "M used; " + free  + "M free; " + total  + "M total")
  }

  /**
   * prints a and returns it.
   */
  def trace[T](a: T) = {println(a); a}

  // this should be a separate trait but Scala is freaking out
  class SeqExtras[T](s: Seq[T]) {
    def argmax(implicit ordering: Ordering[T]) = {
      s.zipWithIndex.reduceLeft( (a,b) => if(ordering.gt(a._1,b._1)) a else b)._2
    }
    def argmin(implicit ordering: Ordering[T]) = {
      s.zipWithIndex.reduceLeft( (a,b) => if(ordering.lt(a._1,b._1)) a else b)._2
    }

    def unfold[U,To](init: U)(f: (U,T)=>U)(implicit cbf: CanBuildFrom[Seq[T], U, To]) = {
      val builder = cbf.apply(s)
      builder.sizeHint(s.size + 1)
      var u = init
      builder += u
      for( t <- s) {
        u = f(u,t)
        builder += u
      }
      builder.result()
    }
  }

  implicit def seqExtras[T](s: Seq[T]) = new SeqExtras(s)

  implicit def arraySeqExtras[T](s: Array[T]) = new SeqExtras(s)


  implicit class AwesomeBitSet(val bs: java.util.BitSet) extends AnyVal {
    def apply(r: Int) = bs.get(r)

    def iterator:Iterator[Int] = new BSIterator(bs)

    def map[U, C](f: Int=>U)(implicit cbf: CanBuildFrom[java.util.BitSet, U, C]) = {
      val r: mutable.Builder[U, C] = cbf(bs)
      r.sizeHint(bs.size)
      iterator foreach { i =>
        r += f(i)
      }

      r.result()
    }

    def foreach[U](f: Int=>U) {
      var i = bs.nextSetBit(0)
      while(i != -1) {
        f(i)
        i = bs.nextSetBit(i+1)
      }

    }

    def &=(other: BitSet) = {
      bs and other
      bs
    }


    def &~=(other: BitSet) = {
      bs andNot other
      bs
    }


    def |=(other: BitSet)= {
      bs or other
      bs
    }


    def ^=(other: BitSet) = {
      bs xor other
      bs
    }

    def |(other: BitSet) = {
      copy |= other
    }

    def &~(other: BitSet) = {
      copy &~= other
    }

    def &(other: BitSet) = {
      copy &= other
    }

    def ^(other: BitSet) = {
      copy ^= other
    }

    def copy = bs.clone().asInstanceOf[java.util.BitSet]

    def nonEmpty = !bs.isEmpty

    def +=(i: Int) = {
      bs.set(i)
      bs
    }
  }

  private class BSIterator(bs: java.util.BitSet) extends Iterator[Int] {
    var currentBit = bs.nextSetBit(0)
    def hasNext: Boolean = currentBit != -1

    def next() = {
      assert(currentBit != -1)
      val cur = currentBit
      currentBit = bs.nextSetBit(cur+1)
      cur
    }
  }

  implicit def _bitsetcbf[U] = new CanBuildFrom[java.util.BitSet, U, Set[U]] {
    def apply(from: BitSet): mutable.Builder[U, Set[U]] = Set.newBuilder[U]
    def apply(): mutable.Builder[U, Set[U]] = Set.newBuilder[U]
  }


  implicit class AwesomeScalaBitSet(val bs: scala.collection.BitSet) extends AnyVal {
    def toJavaBitSet = {
      val jbs = new java.util.BitSet(bs.lastOption.getOrElse(0) + 1)
      bs.foreach(jbs.set(_))
      jbs
    }
  }

}

