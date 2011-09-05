package scalanlp.util.logging

/**
 * 
 * @author dlwh
 */

trait SimpleLogger extends Logger {
  import Logger._
  protected[this] def rawLog(level: Logger.Level, t: =>Any)

  def trace(f: =>Any) = rawLog(TRACE, f)
  def debug(f: =>Any) = rawLog(DEBUG, f)
  def info(f: =>Any) = rawLog(INFO, f)
  def warn(f: =>Any) = rawLog(WARN, f)
  def error(f: =>Any) = rawLog(ERROR, f)
  def fatal(f: =>Any) = rawLog(FATAL, f)
}