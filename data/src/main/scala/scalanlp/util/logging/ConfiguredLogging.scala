package scalanlp.util.logging

import java.io.{FileOutputStream, File}
import scalanlp.config.{ArgumentParser, Configuration}
import scalanlp.util.logging.Logger.Level

/**
 * 
 * @author dlwh
 */

trait ConfiguredLogging extends Logged {
  override val log = ConfiguredLogging[this.type]()
}

object ConfiguredLogging {

  case class LogParams(output: LogOutput = ConsoleOutput, level: Logger.Level = Logger.DEBUG)

  sealed trait LogOutput { def logger: Logger}
  private case object NoOutput extends LogOutput {
    def logger = NullLogger
  }

  private case object ConsoleOutput extends LogOutput {
    def logger = new OutputStreamLogger(System.err, level = Logger.TRACE)
  }

  private case class FileOutput(f: File) extends LogOutput {
    def logger = new OutputStreamLogger(new FileOutputStream(f), level = Logger.TRACE);
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

  var configuration: Configuration = Configuration.fromPropertiesFiles(Seq.empty)

  def apply[T]()(implicit manifest: ClassManifest[T]):Logger = {
    val params = configuration.readIn[LogParams](manifest.getClass.getName+".log")
    val logger = params.output.logger
    val level = params.level

    new ForwardingLogger(logger, level)
  }

}