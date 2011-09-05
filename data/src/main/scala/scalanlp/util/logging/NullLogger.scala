package scalanlp.util.logging

/**
 * 
 * @author dlwh
 */
object NullLogger extends SimpleLogger {
  protected[this] def rawLog(level: Logger.Level, t: => Any) {}
  var level: Logger.Level = Logger.NEVER

}