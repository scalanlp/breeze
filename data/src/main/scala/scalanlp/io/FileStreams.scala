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
package scalanlp;
package io;

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
object FileStreams {
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
