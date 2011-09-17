package scalanlp.util.logging

/*
 Copyright 2011 David Hall

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

import Logger._


/**
 * A logger is anything that can emit log messages of varying severities.
 */
trait Logger {
  def trace(f: =>Any)
  def debug(f: =>Any)
  def info(f: =>Any)
  def warn(f: =>Any)
  def error(f: =>Any)
  def fatal(f: =>Any)

  def level: Logger.Level
  def level_=(level: Logger.Level)
}

object Logger {
  class Level(val severity : Int) {
    def >(that: Logger.Level):Boolean = this.severity > that.severity
    def <(that: Logger.Level):Boolean = this.severity < that.severity
    def <=(that: Logger.Level):Boolean = this.severity <= that.severity
    def >=(that: Logger.Level):Boolean = this.severity >= that.severity
  }
  case object NEVER extends Level(-1000);
  case object FATAL extends Level(0);
  case object ERROR extends Level(10);
  case object WARN extends Level(20);
  case object INFO extends Level(30);
  case object DEBUG extends Level(40);
  case object TRACE extends Level(50);
}