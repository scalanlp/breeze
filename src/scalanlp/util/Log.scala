package scalanlp.util;
import java.io._;

/**
* Class for making logs. Has different levels that can be tuned by setting the
* log level.
* 
* @author dlwh
*/
class Log(os : =>OutputStream, @volatile var level : Log.Level) {
  import Log._;

  /**
  * Defaults to WARNING level or more serious
  */
  def this(os : =>OutputStream) = this(os,Log.WARN);

  /**
  * Defaults to WARNING level or more serious
  */
  def this(f : File) = this(new BufferedOutputStream(new FileOutputStream(f)));

  /**
  * Defaults to WARNING level or more serious
  */
  def this(s : String) = this(new File(s));

  /**
  * Closes this log and closes the file syteam associated with it
  */
  def close() = out.close();

  /**
   * Override to suit your needs.
   */
  def prefix(lvl : Level) = "("+ lvl + ") " + CALLER(2) + ": "

  /**
   * Writes the log message if the severity is high enough. 
   * Otherwise, don't even compute the string.
   */
  def log(lvl : Level)(msg: =>Any) {
    if(level.severity >= lvl.severity)
      out.println(prefix(lvl) + msg.toString);
  }

  def apply(lvl: Level)(msg: =>Any) = log(lvl)(msg)

  private val out = new PrintWriter(os);
}

object Log extends Log(System.err) {
  case class Level(severity : Int);
  case object FATAL extends Level(1);
  case object ERROR extends Level(2);
  case object WARN extends Level(4);
  case object INFO extends Level(8);
  case object DEBUG extends Level(10);
  
  implicit def levelFromInt(x :Int) = Level(x);

  /**
  * Computes the current source file and line number.
  */
  @noinline def LOCATION = { 
    val e = new Exception().getStackTrace()(1);
    e.getFileName() + ":" + e.getLineNumber();
  }

  /** 
  * Computes the source file location of the nth parent. 
  * 0 is equivalent to LOCATION
  */
  @noinline def CALLER(nth : Int) = {
    val e = new Exception().getStackTrace()(nth+1);
    e.getFileName() + ":" + e.getLineNumber();
  }
}
