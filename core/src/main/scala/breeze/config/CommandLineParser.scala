package breeze.config

import collection.mutable.ArrayBuffer
import java.io.{File, FileInputStream}
import java.util.Properties

/**
 * Simple CommandLine Parser that turns options into Configurations
 *
 * -option [value]  (has property option -> value, "true" if no value)
 * -option arg1 +option arg2 (has property option -> arg1,arg2
 * ++file read file as a java properties file. overwrites any other options. No appending.
 * ++ file read file as a java properties file
 *
 *
 * @author dlwh
 */
object CommandLineParser {

  /**
   *
   * @param args command line args
   * @param checkHelp if true and "--help" is set, print help and exit.
   * @param extraArgsAsConfigFiles if true, unprocessed arguments are treated as paths to configuration files.
   * @tparam T
   * @return
   */
  def readIn[T:Manifest](args: IndexedSeq[String],
                              checkHelp: Boolean = true,
                              extraArgsAsConfigFiles: Boolean = true):T = {
    val (baseConfig, files) = parseArguments(args)
    val config:Configuration = if(extraArgsAsConfigFiles) {
      baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    } else {
      require(files.isEmpty, "Unknown options: " + files)
      baseConfig
    }

    if(config.readIn[Boolean]("help", false)) {
      println(breeze.config.GenerateHelp[T](config))
      sys.exit(1)
    }

    val params = try {
      config.readIn[T]("")
    } catch {
      case e:Exception =>
      e.printStackTrace()
      println(breeze.config.GenerateHelp[T](config))
      sys.exit(1)
    }

    params
  }


  /**
   * Reads in arguments as specified above. Returns arguments not bound to an option as second return value.
   * @param args
   * @return
   */
  def parseArguments(args: IndexedSeq[String]):(Configuration,IndexedSeq[String]) = {
    val properties = collection.mutable.Map[String,String]();
    val linear = ArrayBuffer[String]()
    var i = 0;
    while(i < args.length) {
      if((args(i).startsWith("--") || args(i).startsWith("-")) && !isNumber(args(i))) {
        val rawName = args(i).dropWhile('-'==)
        i += 1
        val argument = {
          if(i == args.length) "true"
          else if ( (args(i).startsWith("-") || args(i).startsWith("+")) && !isNumber(args(i))) {
            "true"
          } else {
            val arg = args(i)
            i += 1
            arg
          }
        }
        properties(rawName) = argument
      } else if(args(i) == "++") {
        val ju = new java.util.Properties()
        ju.load(new FileInputStream(new java.io.File(args(i+1))));

        import scala.collection.JavaConversions.propertiesAsScalaMap
        properties ++= ju
        i += 2
      } else if(args(i).startsWith("++")) {
        i += 1
        val rawName = args(i).dropWhile('+'==)
        val ju = new java.util.Properties()
        ju.load(new FileInputStream(new java.io.File(rawName)))

        import scala.collection.JavaConversions.propertiesAsScalaMap
        properties ++= ju
      } else if(args(i).startsWith("+")) { // append
        val rawName = args(i).dropWhile('+'==)
        i += 1
        val argument = {
          if(i == args.length) "true"
          else if(args(i).startsWith("-") || args(i).startsWith("+")) {
            "true"
          } else {
            val arg = args(i)
            i += 1
            arg
          }
        }

        if(properties contains rawName)
          properties(rawName) += ("," + argument)
        else
          properties(rawName) = argument
      } else {
        linear += args(i)
        i += 1
      }
    }

    (Configuration.fromMap(Map.empty ++ properties), linear)
  }

  private def isNumber(s: String) = try { s.toDouble; true } catch {case ex:NumberFormatException => false}
}