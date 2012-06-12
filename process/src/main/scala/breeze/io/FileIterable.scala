package breeze.io

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

/**
 * A File iterable is an iterable backed by a file.
 * @author dlwh
 */
class FileIterable[+T](file: File) extends Iterable[T] {
  def iterator:Iterator[T] = {
    val in = new ObjectInputStream(new BufferedInputStream(new GZIPInputStream(new FileInputStream(file))));
    var closed = false;
    new Iterator[T] {
      var stash : Option[T] = None;
      def next = {
        if(stash == None) consume();
        val r = stash.get
        stash = None
        r
      }

      def hasNext = stash != None || (!closed && { consume(); stash != None})

      private def consume() = try {
        stash = Some(in.readObject().asInstanceOf[T]);
        true;
      } catch {
        case e: EOFException =>  in.close(); closed = true; false
      }

      override protected def finalize() = {
        if(!closed) in.close();
      }
    }

  }
}

object FileIterable {
  def write[T](it: Iterable[T], file: File) = {
    val out = new ObjectOutputStream(new BufferedOutputStream(new GZIPOutputStream(new FileOutputStream(file))));
    for(t <- it) out.writeObject(t);
    out.close();
    new FileIterable[T](file);
  }
}