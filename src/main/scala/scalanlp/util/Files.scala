package scalanlp.util

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


import java.io._;
import scala.collection.mutable.ArrayBuffer;

/**
 * This contains several convenience routines that wrap file
 * reading and writing operations with niceities such as 
 * exception handling.
 *
 * @author yeh1
 */
object FileUtils {

  /**
   * Simply opens the target file, opens a BufferedReader on it, and
   * passes the reader to the operation.  Handles exception wrapping.
   */
  def read(file:File)(operation:BufferedReader=>Any):Boolean = {
    try {
      var in = new BufferedReader(new FileReader(file));
      operation(in);
      true;
    } catch {
      case e:Exception => {
	System.err.println("FileUtils.read: Error "+e.getMessage());
	e.printStackTrace
      }
      false;
    }
  }

  /**
   * Given a target file, and an operation that accepts
   * a single String line, reads the file in line by line,
   * applying the given operation until the file is exhausted.
   */
  def readByLine(file:File)(operation:String=>Any):Boolean = {
    try {
      var in = new BufferedReader(new FileReader(file));
      var line:String = in.readLine();
      while (line != null) {
	operation(line);
	line = in.readLine();
      }
      true;
    } catch {
      case e:Exception => {
	System.err.println("FileUtils.readByLine: Error "+e.getMessage());
	e.printStackTrace
      }
      false;
    }
  }

  def readIntoArray(file:File):Seq[String] = {
    var ret = new ArrayBuffer[String]();
    readByLine(file) {
      (line) => {
	ret += line;
      }
    }
    ret.toList;
  }

  /**
   * Given a target file and an operation that accepts a writer, 
   * simply sets up an exception wrapped writer and calls the operation
   * with the writer.
   */
  def write(file:File)(operation: Writer=>Unit):Boolean  = {
    try {
      var out = new BufferedWriter(new FileWriter(file));
      operation(out);
      out.close
      true
    } catch {
      case e:Exception => {
	System.err.println("FileUtils.write: Error "+e.getMessage());
	e.printStackTrace
      }
      false;
    }
  }
}

// please ant
object Files;
