package breeze.util.logging

import java.io.{FileOutputStream, File}
import breeze.config.{ArgumentParser, Configuration}
import breeze.util.logging.Logger.Level

/**
 * Provides a log from ConfigueredLogging's factory for this type.
 * @author dlwh
 */
trait ConfiguredLogging extends Logged {
  override val log = ConfiguredLogging[this.type]()
}

object ConfiguredLogging {

  case class LogParams(output: LogOutput = ConsoleOutput, level: Logger.Level = Logger.INFO) {
    def logger = output.logger(level)
  }

  sealed trait LogOutput { def logger(level: Level): Logger}
  private case object NoOutput extends LogOutput {
    def logger(level: Level) = NullLogger
  }

  private case object ConsoleOutput extends LogOutput {
    def logger(level: Level) = new OutputStreamLogger(System.err, level)
  }

  private case class FileOutput(f: File) extends LogOutput {
    def logger(level: Level) = new OutputStreamLogger(new FileOutputStream(f), level);
  }

  ArgumentParser addArgumentParser new ArgumentParser[LogOutput] {
    def parse(arg: String) = arg match {
      case "none" | "None" | "NONE" | "NoOutput" => NoOutput
      case "console" | "Console" | "CONSOLE" => ConsoleOutput
      case _ => new FileOutput(new File(arg))
    }
  }
  ArgumentParser addArgumentParser new ArgumentParser[Logger.Level] {
    def parse(arg: String) = arg.toLowerCase match {
      case "never" => Logger.NEVER
      case "fatal" => Logger.FATAL
      case "error" => Logger.ERROR
      case "warn" => Logger.WARN
      case "info" => Logger.INFO
      case "debug" => Logger.DEBUG
      case "trace" => Logger.TRACE
      case "" => Logger.TRACE
    }
  }

  /**
   * Sets the configuration for future reads from the log.
   *
   * <class name>.log.output = {none,console,some file name}
   * <class name>.log.level = (some level, e.g. never, fatal, debug)
   */
  var configuration: Configuration = Configuration.fromPropertiesFiles(Seq.empty)

  def apply[T]()(implicit manifest: ClassManifest[T]):Logger = {
    configuration.readIn[LogParams](manifest.getClass.getName+".log").logger
  }

}