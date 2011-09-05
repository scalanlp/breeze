package scalanlp.util
package logging

import java.io.{PrintWriter, OutputStream}

/**
 * 
 * @author dlwh
 */

class OutputStreamLogger(outputStream: OutputStream, var level: Logger.Level = Logger.INFO) extends SimpleLogger {
  private val out = new PrintWriter(outputStream);

  /**
   * Override to suit your needs.
   */
  def prefix(lvl : Logger.Level) = "("+ lvl + ") (" + CALLER(2) + ") "

  protected[this] def rawLog(level: Logger.Level, msg: => Any) = {
    if(level.severity > this.level.severity) {
      out.println(prefix(level) + msg.toString);
      out.flush()
    }

  }
}