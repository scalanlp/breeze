package scalanlp.util.logging

import scalanlp.util.logging.Logger.Level

/**
 * 
 * @author dlwh
 */
object NullLogger extends Logger {
  def trace(f: => Any) {}

  def debug(f: => Any) {}

  def info(f: => Any) {}

  def warn(f: => Any) {}

  def error(f: => Any) {}

  def fatal(f: => Any) {}

  def level_=(level: Level) {}

  def level = Logger.NEVER
}