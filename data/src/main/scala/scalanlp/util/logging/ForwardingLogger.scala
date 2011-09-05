package scalanlp.util.logging

import Logger._
/**
 * 
 * @author dlwh
 */

class ForwardingLogger(logger: Logger, var level: Logger.Level) extends Logger {
  def trace(f: => Any) = if(level >= TRACE) trace(f)
  def debug(f: => Any) = if(level >= DEBUG) debug(f)
  def info(f: => Any) = if(level >= INFO) info(f)
  def warn(f: => Any) = if(level >= WARN) warn(f)
  def error(f: => Any) = if(level >= ERROR) error(f)
  def fatal(f: => Any) = if(level >= FATAL) fatal(f)

}