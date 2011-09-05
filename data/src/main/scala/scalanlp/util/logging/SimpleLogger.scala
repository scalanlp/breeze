package scalanlp.util.logging

import scalanlp.util._

/**
 * 
 * @author dlwh
 */

trait SimpleLogger extends Logger {
  import Logger._


  /**
   * Override to suit your needs.
   */
  def prefix(lvl : Logger.Level) = "("+ lvl + ") (" + CALLER(5) + ") "

  private def log(level: Logger.Level, t: =>Any) {
    if(level <= this.level) {
      rawLog(prefix(level) + t.toString)
    }
  }
  protected[this] def rawLog(str: String)

  def trace(f: =>Any) = log(TRACE, f)
  def debug(f: =>Any) = log(DEBUG, f)
  def info(f: =>Any) = log(INFO, f)
  def warn(f: =>Any) = log(WARN, f)
  def error(f: =>Any) = log(ERROR, f)
  def fatal(f: =>Any) = log(FATAL, f)
}