package scalanlp.util.logging

/**
 * 
 * @author dlwh
 */
object ConsoleLogger extends OutputStreamLogger(System.err) {

}

trait ConsoleLogging extends Logged {
  override val log = ConsoleLogger
}