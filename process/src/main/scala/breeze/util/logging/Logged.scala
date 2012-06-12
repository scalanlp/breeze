package breeze.util.logging

/**
* Uniform logging interface. By default, nothing gets logged.
*
* Clients of classes that extend Logged can mixin another trait, like ConsoleLogging,
* or override log themselves
* @author dlwh
*/
trait Logged {
  val log: Logger = NullLogger;
}