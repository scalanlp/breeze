package scalanlp.util
package logging

import java.io.{PrintWriter, OutputStream}

/**
 * Writes log output to an output stream
 * @author dlwh
 */
class OutputStreamLogger(outputStream: OutputStream, var level: Logger.Level = Logger.INFO) extends SimpleLogger {
  private val out = new PrintWriter(outputStream);

  protected[this] override def rawLog(msg: String) = {
    out.println(msg)
    out.flush()


  }
}