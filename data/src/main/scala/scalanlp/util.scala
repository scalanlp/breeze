package scalanlp

import java.io._
import util.{IteratorImplicits, DoubleImplicits}
;

/**
 * Adds a bunch of implicits and things that are generically useful.
 * @author dlwh
 */
package object util extends DoubleImplicits with IteratorImplicits {
  def readObject[T](loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val result = oin.readObject().asInstanceOf[T]
    oin.close();
    result;
  }


  def writeObject[T](out: File, parser: T): Unit = {
    val stream = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(out)));
    stream.writeObject(parser);
    stream.close();
  }

  /**
   * You can write TODO in your code, and get an exception at runtime for any expression.
   */
  def TODO = error("TODO");

  /**
   * You can write XXX in your code and get an exception at runtime for any expression.
   */
  def XXX = error("XXX");
  /**
   * Similar to the TODO expression, except this one is for types.
   */
  type TODO = Nothing;


}