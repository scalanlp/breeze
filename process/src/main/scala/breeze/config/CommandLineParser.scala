package breeze.config

import collection.mutable.ArrayBuffer
import java.io.FileInputStream
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

  private def isNumber(s: String) = try { s.toDouble; true } catch {case ex => false}

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

}