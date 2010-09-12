/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package scalanlp.serialization

import java.io.File;
import java.io.{FileInputStream,FileOutputStream};
import java.io.{BufferedInputStream,BufferedOutputStream};
import java.util.zip.{GZIPInputStream,GZIPOutputStream};

/**
 * Gets input and output streams to a file, wrapping them in
 * GZIP streams if the file ends with .gz.
 *
 * @author dramage
 */
private[serialization] object FileStreams {
  def input(file : File) = {
    val fis = new BufferedInputStream(new FileInputStream(file));

    if (file.getPath.toLowerCase.endsWith(".gz")) {
      new GZIPInputStream(fis)
    } else { fis };
  }

  def output(file : File) = {
    val fos = new BufferedOutputStream(
      new FileOutputStream(file));

    if (file.getPath.toLowerCase.endsWith(".gz")) {
      new GZIPOutputStream(fos)
    } else { fos };
  }
}
