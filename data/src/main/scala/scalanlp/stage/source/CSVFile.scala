/*
 Copyright 2009 David Hall, Daniel Ramage

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
package scalanlp.stage.source;

import java.io.File;

import scalanlp.stage.{Parcel,Batch,History};

/**
 * A CSVFile acts as a source of Array[String].  Uses the scalax CSV parser
 * under the hood, which correctly handles multi-line cells and embedded
 * &quot; markers.
 *
 * @author dramage
 */
case class CSVFile(path : String) extends File(path) {
  private def reader() = {
    val fis = new java.io.BufferedInputStream(
      new java.io.FileInputStream(this));

    val is =
      if (path.toLowerCase.endsWith(".gz")) {
        new java.util.zip.GZIPInputStream(fis)
      } else { fis };

    new java.io.BufferedReader(new java.io.InputStreamReader(is));
  }

  def rows : Iterable[Seq[String]] = new Iterable[Seq[String]] {
    override def iterator =
      new CSVIterator(reader());
  }

  implicit def asParcel : Parcel[Batch[Seq[String]]] = {
    Parcel(History.Origin(toString), Batch.fromIterable(rows));
  }

  override def toString =
    "CSVFile(\""+path+"\")";
}

/**
 * Companion object for CSVFiles that includes an implicit conversion
 * to a parcel and a format method that correctly escapes and joins
 * a sequence of strings.
 * 
 * @author dramage
 */
object CSVFile {
  /** CSVFile that points to the given file. */
  def apply(file : File) =
    new CSVFile(file.getPath);
  
  /** CSVFile that points to a file within the given base folder. */
  def apply(base : File, name : String) =
    new CSVFile(new File(base, name).getPath); 
  
  /** Formats the given sequence of strings as well-formed line of CSV. */
  def format(seq : Iterable[String]) : String = {
    ( for (field <- seq) yield {
        if (field.contains('\n') || field.contains('\r') || field.contains('\"') || field.contains(",")) {
          "\"" + field.replaceAll("\"","\"\"") + "\"";
        } else {
          field;
        }
      }
    ).mkString(",");
  }
}



/**
 * <em>From scalax 0.1:</em>
 * 
 * An iterator interface to a CSV data stream: generates an array of strings
 * representing each row. Note that the underlying Reader should be closed
 * manually (or by a ManagedResource) if iteration is not completed.
 */
class CSVIterator(csv : java.io.BufferedReader) extends Iterator[Seq[String]] {
  /** The separator character, a comma by default, but could be overridden to
   * any character which is not whitespace or '"'. */
  def sep = ','

  /** The number of fields each row should have. Default is 0, which means
   * any number. */
  def arity = 0

  /** If true, ignore blank lines and treat commentStart outside quotes as
   * starting a comment line. Default is false. */
  def comments = false

  /** The comment start character. Default '#'. Only effective if comments is
   * true. */
  def commentStart = '#'

  private var nextLine = ""
  private var lineNo = 0
  getNext()

  private def getNext() : Unit = {
    nextLine = csv.readLine()
    lineNo += 1
    if(nextLine == null) {
      csv.close()
    } else if(comments) {
      var i = 0
      val len = nextLine.length
      while(i < len && Character.isWhitespace(nextLine.charAt(i))) i += 1
      if(i == len || nextLine.charAt(i) == commentStart) getNext()
    }
  }

  def hasNext : Boolean = (nextLine != null)
  def next : Seq[String] = {
    var fields = new scala.collection.mutable.ArrayBuffer[String];
    var chars = nextLine.toCharArray()
    var len = chars.length
    var i = -1
    while(i < len) {
      val dropComment = i == -1 || chars(i) != sep
      i += 1
      while(i < len && Character.isWhitespace(chars(i))) i += 1
      if(i < len && chars(i) == '"') {
        // Quoted field
        val field = new java.lang.StringBuilder();
        i += 1
        var done = false
        while(!done) {
          val start = i
          while(i < len && chars(i) != '"') i += 1
          field.append(chars, start, i - start)
          if(i == len) {
            field.append('\n')
            nextLine = csv.readLine();
            if(nextLine == null) {
              csv.close()
              throw new CSVParseException(lineNo+":"+(i + 1)+": Mismatched quotes", lineNo)
            }
            chars = nextLine.toCharArray()
            len = chars.length
            lineNo += 1
            i = 0
          } else if(i + 1 < len && chars(i + 1) == '"') {
            field.append('"')
            i = i + 2
          } else {
            done = true
          }
        }
        i += 1
        while(i < len && chars(i) != sep && !(comments && chars(i) == commentStart)) {
          if(!Character.isWhitespace(chars(i)))
            throw new CSVParseException(lineNo+":"+(i + 1)+
                                     ": Garbage after close quote", lineNo)
          i += 1
        }
        if(i < len && comments && chars(i) == commentStart) i -= 1
        fields += field.toString()
      } else if(comments && i < len && chars(i) == commentStart) {
        // Comment
        if(!dropComment) fields += ""
        i = len
      } else {
        // Non-quoted field
        val start = i
        while(i < len && chars(i) != sep && !(comments && chars(i) == commentStart)) i += 1
        var end = i - 1
        if(i < len && comments && chars(i) == commentStart) i -= 1
        while(end >= start && Character.isWhitespace(chars(end))) end = end - 1
        fields += new String(chars, start, end - start + 1)
      }
    }
    if(arity != 0 && fields.length != arity)
      throw new CSVParseException(lineNo+":1: Found "+fields.length+
                               " fields but was expecting "+arity, lineNo)
    getNext()
    fields;
  }
}

class CSVParseException(msg : String, val lineNo : Int) extends RuntimeException(msg);
