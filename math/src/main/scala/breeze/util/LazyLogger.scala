package breeze.util

import org.slf4j.Logger

/**
 * A logger that only evaluates parameters lazily if the corresponding log level is enabled.
 */
class LazyLogger(val logger: Logger) extends Serializable {

  def info(msg: => String): Unit = {
    if (logger.isInfoEnabled) logger.info(msg)
  }

  def debug(msg: => String): Unit = {
    if (logger.isDebugEnabled) logger.debug(msg)
  }

  def trace(msg: => String): Unit = {
    if (logger.isTraceEnabled) logger.trace(msg)
  }

  def warn(msg: => String): Unit = {
    if (logger.isWarnEnabled) logger.warn(msg)
  }

  def error(msg: => String): Unit = {
    if (logger.isErrorEnabled) logger.error(msg)
  }

  def info(msg: => String, throwable: Throwable): Unit = {
    if (logger.isInfoEnabled) logger.info(msg, throwable)
  }

  def debug(msg: => String, throwable: Throwable): Unit = {
    if (logger.isDebugEnabled) logger.debug(msg, throwable)
  }

  def trace(msg: => String, throwable: Throwable): Unit = {
    if (logger.isTraceEnabled) logger.trace(msg, throwable)
  }

  def warn(msg: => String, throwable: Throwable): Unit = {
    if (logger.isWarnEnabled) logger.warn(msg, throwable)
  }

  def error(msg: => String, throwable: Throwable): Unit = {
    if (logger.isErrorEnabled) logger.error(msg, throwable)
  }
}
