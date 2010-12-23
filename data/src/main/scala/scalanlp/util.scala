package scalanlp

import java.io._
import util.{IteratorImplicits, DoubleImplicits}
;

/**
 * 
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

  def TODO = error("TODO");
  def XXX = error("XXX");
  type TODO = Nothing;


}