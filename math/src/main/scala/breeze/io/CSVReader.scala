package breeze.io

import java.io._
import au.com.bytecode.opencsv.{CSVReader => OpenCSVReader, CSVWriter => OpenCSVWriter}

import scala.collection.compat.immutable.ArraySeq
import scala.collection.compat._

/**
 * Just a simple wrapper for OpenCSV's csvreader.
 * @author dlwh
 */
object CSVReader {
  def read(
      input: Reader,
      separator: Char = ',',
      quote: Char = '"',
      escape: Char = '\\',
      skipLines: Int = 0): IndexedSeq[IndexedSeq[String]] = {
    iterator(input, separator, quote, escape, skipLines).toIndexedSeq
  }

  def parse(
      input: String,
      separator: Char = ',',
      quote: Char = '"',
      escape: Char = '\\',
      skipLines: Int = 0): IndexedSeq[IndexedSeq[String]] = {
    read(new StringReader(input), separator, quote, escape, skipLines)

  }

  def iterator(input: Reader, separator: Char = ',', quote: Char = '"', escape: Char = '\\', skipLines: Int = 0): Iterator[IndexedSeq[String]] = {
    val rdr = new OpenCSVReader(input, separator, quote, escape, skipLines)
    new Iterator[IndexedSeq[String]] {
      var _next = rdr.readNext()
      def hasNext: Boolean = _next ne null

      def next(): IndexedSeq[String] = {
        if (!hasNext) throw new NoSuchElementException("Next on empty iterator.")
        val x = _next
        _next = rdr.readNext

        ArraySeq.unsafeWrapArray(x)
      }

      def close(): Unit = {
        rdr.close()
        _next = null
      }
    }
  }

}

/**
 * Just a simple wrapper for OpenCSV's csvreader.
 * @author dlwh
 */
object CSVWriter {
  def write(
      output: Writer,
      mat: TraversableOnce[IndexedSeq[String]],
      separator: Char = ',',
      quote: Char = '"',
      escape: Char = '\\'): Unit = {
    val writer = new OpenCSVWriter(output, separator, quote, escape)
    import scala.collection.JavaConverters._
    mat match {
      case x: Seq[Seq[String]] => writer.writeAll(x.map(_.toArray).asJava)
      case _ =>
        for (l <- mat.iterator) {
          writer.writeNext(l.toArray)
        }
    }
    writer.flush()
  }

  def writeFile(
      file: File,
      mat: IndexedSeq[IndexedSeq[String]],
      separator: Char = ',',
      quote: Char = '"',
      escape: Char = '\\'): Unit = {
    val out = new FileWriter(file)
    write(out, mat, separator, quote, escape)
    out.close()
  }

  def mkString(
      mat: IndexedSeq[IndexedSeq[String]],
      separator: Char = ',',
      quote: Char = '"',
      escape: Char = '\\'): String = {
    val out = new StringWriter
    write(out, mat, separator, quote, escape)
    out.toString
  }

}
