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
package breeze;
package io;

import java.io.File;
import java.io.{InputStream, OutputStream};
import java.io.{FileInputStream, FileOutputStream};
import java.io.{BufferedInputStream, BufferedOutputStream};
import java.util.zip.{GZIPInputStream, GZIPOutputStream};

/**
 * Gets input and output streams to a file, wrapping them in
 * GZIP streams if the file ends with .gz.
 *
 * @author dramage
 */
object FileStreams {

  /** Use a 16k buffer size. */
  val BUFFER_SIZE = 16 * 1024;

  /**
   * Gets an input stream with proper buffering (minimum 16k) for the given
   * file, automatically gunziping if the file name ends in .gz.
   */
  def input(path: File): InputStream = {
    val fis = new FileInputStream(path);
    try {
      if (path.getName.endsWith(".gz")) {
        new BufferedInputStream(new GZIPInputStream(fis, BUFFER_SIZE), BUFFER_SIZE);
      } else {
        new BufferedInputStream(fis, BUFFER_SIZE);
      }
    } catch {
      case ex: Throwable =>
        fis.close();
        throw ex;
    }
  }

  /**
   * Gets an output stream writing to the given file with proper buffering
   * (minimum 16k), automatically gziping if the file name ends in .gz.
   */
  def output(path: File): OutputStream = {
    val fos = new FileOutputStream(path);
    try {
      if (path.getName.endsWith(".gz")) {
        new BufferedOutputStream(new GZIPOutputStream(fos, BUFFER_SIZE), BUFFER_SIZE);
      } else {
        new BufferedOutputStream(fos, BUFFER_SIZE);
      }
    } catch {
      case ex: Throwable =>
        fos.close();
        throw ex;
    }
  }
}
