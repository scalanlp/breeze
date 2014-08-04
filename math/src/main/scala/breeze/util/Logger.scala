package breeze.util

import org.slf4j.Logger

/**
 * A logger that only evaluates parameters lazily if the corresponding log level is enabled.
 */
class LazyLogger(log: Logger) extends Serializable {

  def info(msg: => String) {
    if (log.isInfoEnabled) log.info(msg)
  }

  def debug(msg: => String) {
    if (log.isDebugEnabled) log.debug(msg)
  }

  def trace(msg: => String) {
    if (log.isTraceEnabled) log.trace(msg)
  }

  def warn(msg: => String) {
    if (log.isWarnEnabled) log.warn(msg)
  }

  def error(msg: => String) {
    if (log.isErrorEnabled) log.error(msg)
  }

  def info(msg: => String, throwable: Throwable) {
    if (log.isInfoEnabled) log.info(msg, throwable)
  }

  def debug(msg: => String, throwable: Throwable) {
    if (log.isDebugEnabled) log.debug(msg, throwable)
  }

  def trace(msg: => String, throwable: Throwable) {
    if (log.isTraceEnabled) log.trace(msg, throwable)
  }

  def warn(msg: => String, throwable: Throwable) {
    if (log.isWarnEnabled) log.warn(msg, throwable)
  }

  def error(msg: => String, throwable: Throwable) {
    if (log.isErrorEnabled) log.error(msg, throwable)
  }
}
