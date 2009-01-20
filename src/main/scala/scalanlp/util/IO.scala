package scalanlp.util;

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

object IO {
  /**
   * Convenience wrapper for pulling in data, by line, from STDIN.
   */
  def readStdInByLine(operation:String=>Any):Boolean = {
    try {
      var in = new BufferedReader(new InputStreamReader(System.in));
      var line = in.readLine();
      while (line != null) {
	operation(line);
	line = in.readLine();
      }
      true;
    } catch {
      case e:Exception => {
	System.err.println("IO.readStdInByLine error, msg="+e.getMessage());
	e.printStackTrace();
      }
      false;
    }
  }
}
