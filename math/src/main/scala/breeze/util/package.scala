package breeze

import java.util.zip._
import java.io._

import scala.collection.generic._
import scala.collection.mutable
import java.util.BitSet

import scala.collection.compat._
import scala.collection.compat.immutable.ArraySeq

/**
 *
 * @author dlwh
 */
package object util {

  /**
   * Deserializes an object using java serialization
   */
  def readObject[T](loc: File): T = readObject(loc, false)

  /**
   * Deserializes an object using java serialization
   */
  def readObject[T](loc: File, ignoreSerialVersionUID: Boolean) = {
    val stream = new BufferedInputStream(new GZIPInputStream(new FileInputStream(loc)))
    val oin = nonstupidObjectInputStream(stream, ignoreSerialVersionUID)
    try {
      oin.readObject().asInstanceOf[T]
    } finally {
      oin.close()
    }
  }

  def serializeToBytes[T](obj: T): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val objOut = new ObjectOutputStream(out)
    objOut.writeObject(obj)
    objOut.close()
    out.close()
    out.toByteArray
  }

  def deserializeFromBytes[T](bytes: Array[Byte]): T = {
    val in = new ByteArrayInputStream(bytes)
    val objIn = new ObjectInputStream(in)
    try {
      objIn.readObject().asInstanceOf[T]
    } finally {
      objIn.close()
    }
  }

  /**
   * For reasons that are best described as asinine, ObjectInputStream does not take into account
   * Thread.currentThread.getContextClassLoader. This fixes that.
   *
   * @param stream
   * @param ignoreSerialVersionUID this is not a safe thing to do, but sometimes...
   * @return
   */
  def nonstupidObjectInputStream(stream: InputStream, ignoreSerialVersionUID: Boolean = false): ObjectInputStream = {
    new ObjectInputStream(stream) with SerializableLogging {
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

      // from http://stackoverflow.com/questions/1816559/make-java-runtime-ignore-serialversionuids
      override protected def readClassDescriptor(): ObjectStreamClass = {
        var resultClassDescriptor = super.readClassDescriptor(); // initially streams descriptor
        if (ignoreSerialVersionUID) {

          var localClass: Class[_] = null; // the class in the local JVM that this descriptor represents.
          try {
            localClass = Class.forName(resultClassDescriptor.getName)
          } catch {
            case e: ClassNotFoundException =>
              logger.error("No local class for " + resultClassDescriptor.getName, e)
              return resultClassDescriptor
          }

          val localClassDescriptor = ObjectStreamClass.lookup(localClass)
          // only if class implements serializable
          if (localClassDescriptor != null) {
            val localSUID = localClassDescriptor.getSerialVersionUID
            val streamSUID = resultClassDescriptor.getSerialVersionUID
            if (streamSUID != localSUID) {
              val s = new StringBuffer("Overriding serialized class version mismatch: ")
              s.append("local serialVersionUID = ").append(localSUID)
              s.append(" stream serialVersionUID = ").append(streamSUID)
              val e = new InvalidClassException(s.toString())
              logger.error(s"Potentially Fatal Deserialization Operation while deserializing $localClass", e);
              resultClassDescriptor = localClassDescriptor; // Use local class descriptor for deserialization
            }

          }
        }
        resultClassDescriptor
      }
    }
  }

  implicit class FileUtil(val sc: StringContext) extends AnyVal {

    def file(args: Any*): File = new File(sc.s(args: _*))
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
   * Similar to ???, but for types
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
  @noinline def CALLER(nth: Int) = {
    val e = new Exception().getStackTrace()(nth + 1)
    e.getFileName() + ":" + e.getLineNumber()
  }

  /**
   * Returns a string with info about the available and used space.
   */
  def memoryString = {
    val r = Runtime.getRuntime
    val free = r.freeMemory / (1024 * 1024)
    val total = r.totalMemory / (1024 * 1024)
    s"${(total - free)}M used; ${free}M free; ${total}M total"
  }

  /**
   * prints a and returns it.
   */
  def trace[T](a: T) = { println(a); a }

  // this should be a separate trait but Scala is freaking out
  implicit class SeqExtras[T](s: Seq[T]) {
    def argmax(implicit ordering: Ordering[T]) = {
      s.zipWithIndex.reduceLeft((a, b) => if (ordering.gt(a._1, b._1)) a else b)._2
    }
    def argmin(implicit ordering: Ordering[T]) = {
      s.zipWithIndex.reduceLeft((a, b) => if (ordering.lt(a._1, b._1)) a else b)._2
    }

    def unfold[U, To](init: U)(f: (U, T) => U)(implicit cbf: BuildFrom[Seq[T], U, To]) = {
      val builder = cbf.newBuilder(s)
      builder.sizeHint(s.size + 1)
      var u = init
      builder += u
      for (t <- s) {
        u = f(u, t)
        builder += u
      }
      builder.result()
    }
  }

  implicit def arraySeqExtras[T](s: Array[T]): SeqExtras[T] = new SeqExtras(ArraySeq.unsafeWrapArray(s))

  implicit class AwesomeBitSet(val bs: java.util.BitSet) extends AnyVal {
    def apply(r: Int) = bs.get(r)

    def iterator: Iterator[Int] = new BSIterator(bs)

    def map[U, C](f: Int => U)(implicit cbf: BuildFrom[java.util.BitSet, U, C]) = {
      val r: mutable.Builder[U, C] = cbf.newBuilder(bs)
      r.sizeHint(bs.size)
      iterator.foreach { i =>
        r += f(i)
      }

      r.result()
    }

    def foreach[U](f: Int => U): Unit = {
      var i = bs.nextSetBit(0)
      while (i != -1) {
        f(i)
        i = bs.nextSetBit(i + 1)
      }

    }

    def &=(other: BitSet) = {
      bs.and(other)
      bs
    }

    def &~=(other: BitSet) = {
      bs.andNot(other)
      bs
    }

    def |=(other: BitSet) = {
      bs.or(other)
      bs
    }

    def ^=(other: BitSet) = {
      bs.xor(other)
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
      currentBit = bs.nextSetBit(cur + 1)
      cur
    }
  }

  implicit def _bitsetcbf[U]: BuildFrom[java.util.BitSet, U, Set[U]] =
    new BuildFrom[java.util.BitSet, U, Set[U]] {
      override def fromSpecific(from: java.util.BitSet)(it: IterableOnce[U]): Set[U] = Set.empty[U] ++ it
      override def newBuilder(from: java.util.BitSet): mutable.Builder[U, Set[U]] = Set.newBuilder

      def apply(): mutable.Builder[U, Set[U]] = Set.newBuilder[U]
    }

  implicit class AwesomeScalaBitSet(val bs: scala.collection.BitSet) extends AnyVal {
    def toJavaBitSet: java.util.BitSet = {
      val jbs = new java.util.BitSet(bs.lastOption.getOrElse(0) + 1)
      bs.foreach(jbs.set(_))
      jbs
    }
  }

}
