package breeze

import java.io._
import util.{IteratorImplicits, DoubleImplicits}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import scala.collection.generic.CanBuildFrom

/**
 * Adds a bunch of implicits and things that are generically useful.
 * @author dlwh
 */
package object util extends DoubleImplicits with IteratorImplicits {
  /**
   * Deserializes an object using java serialization
   */
  def readObject[T](loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new GZIPInputStream(new FileInputStream(loc))));
    val result = oin.readObject().asInstanceOf[T]
    oin.close();
    result;
  }

  /**
   * Serializes an object using java serialization
   */
  def writeObject[T](out: File, parser: T): Unit = {
    val stream = new ObjectOutputStream(new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream(out))));
    stream.writeObject(parser);
    stream.close();
  }

  /**
   * You can write TODO in your code, and get an exception at runtime for any expression.
   */
  def TODO = sys.error("TODO (Not implemented)");

  /**
   * You can write XXX in your code and get an exception at runtime for any expression.
   */
  def XXX = sys.error("XXX Not Implemented");
  /**
   * Similar to the TODO expression, except this one is for types.
   */
  type TODO = Nothing;

  /**
   * Computes the current source file and line number.
   */
  @noinline def LOCATION = {
    val e = new Exception().getStackTrace()(1);
    e.getFileName() + ":" + e.getLineNumber();
  }

  /**
   * Computes the source file location of the nth parent.
   * 0 is equivalent to LOCATION
   */
  @noinline def CALLER(nth : Int) = {
    val e = new Exception().getStackTrace()(nth+1);
    e.getFileName() + ":" + e.getLineNumber();
  }

  /**
   * Returns a string with info about the available and used space.
   */
  def memoryString = {
    val r = Runtime.getRuntime;
    val free = r.freeMemory / (1024 * 1024);
    val total = r.totalMemory / (1024 * 1024);
    ((total - free) + "M used; " + free  + "M free; " + total  + "M total");
  }

  /**
   * prints a and returns it.
   */
  def trace[T](a: T) = {println(a); a}

  // this should be a separate trait but Scala is freaking out
  class SeqExtras[T](s: Seq[T]) {
    def argmax(implicit ordering: Ordering[T]) = {
      s.zipWithIndex.reduceLeft( (a,b) => if(ordering.gt(a._1,b._1)) a else b)._2;
    }
    def argmin(implicit ordering: Ordering[T]) = {
      s.zipWithIndex.reduceLeft( (a,b) => if(ordering.lt(a._1,b._1)) a else b)._2;
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

  implicit def seqExtras[T](s: Seq[T]) = new SeqExtras(s);

  implicit def arraySeqExtras[T](s: Array[T]) = new SeqExtras(s);
}