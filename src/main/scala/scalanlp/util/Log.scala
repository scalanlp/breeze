package scalanlp.util;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

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
  def prefix(lvl : Level) = "("+ lvl + ") (" + CALLER(2) + ") "

  /**
   * Writes the log message if the severity is high enough.
   * Otherwise, don't even compute the string.
   */
  def log(lvl : Level)(msg: =>Any) {
    if(level.severity >= lvl.severity) {
      out.println(prefix(lvl) + msg.toString);
      out.flush();
    }
  }

  def apply(lvl: Level)(msg: =>Any) = {
    if(level.severity >= lvl.severity) {
      out.println(prefix(lvl) + msg.toString);
      out.flush();
    }
  }

  private val out = new PrintWriter(os);
}

object Log {
  
  val globalLog = new Log(System.err,Log.INFO)
  
  case class Level(severity : Int);
  case object NEVER extends Level(-1000);
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

object NullLog extends Log(System.err,Log.NEVER) {
  override def log(l:Log.Level)(x: =>Any) {}
}

/**
* Uniform logging interface. By default, nothing gets logged.
*
* Clients of classes that extend Logged can mixin another trait, like ConsoleLogging,
* or override log themselves
* @author dlwh
*/
trait Logged {
  val log: Log = NullLog;
}

/**
* Logs all output to System.err
* @author dlwh
*/
trait ConsoleLogging extends Logged {
  override val log = Log.globalLog;
}
