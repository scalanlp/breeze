package breeze.util.logging

/**
 * Logs to stderr
 * @author dlwh
 */
object ConsoleLogger extends OutputStreamLogger(System.err);

/**
 * Uses the ConsoleLogger
 */
trait ConsoleLogging extends Logged {
  override val log = ConsoleLogger
}