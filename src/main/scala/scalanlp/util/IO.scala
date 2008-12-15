package scalanlp.util;

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
