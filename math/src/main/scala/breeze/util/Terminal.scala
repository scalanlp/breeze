package breeze.util

/*
 Copyright 2012 David Hall

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

/**
 *
 * @author dlwh, dramage
 */
object Terminal {
  /** The width of the console, or 80 if it can't be discovered. */
  lazy val terminalWidth : Int = {
    // this ugly try-catch is here to use scala's built-in jline,
    // which only exists in scala > 2.9
    try {
      type Terminal = { def getWidth() : Int; def getHeight() : Int; }
      val terminal = Class.forName("scala.tools.jline.TerminalFactory").
        getMethod("create").invoke(null).asInstanceOf[Terminal]
      terminal.getWidth()
    } catch {
      case _ : Throwable => try {
        type Terminal = { def getTerminalWidth() : Int; def getTerminalHeight() : Int; }
        val terminal = Class.forName("jline.Terminal").
          getMethod("getInstance").invoke(null).asInstanceOf[Terminal]
        terminal.getTerminalWidth()
      } catch {
        case _ : Throwable => 80
      }
    }
  }

  /** The height of the console, or 24 if it can't be discovered. */
  lazy val terminalHeight : Int = {
    // this ugly try-catch is here to use scala's built-in jline,
    // which only exists in scala > 2.9
    try {
      type Terminal = { def getWidth() : Int; def getHeight() : Int; }
      val terminal = Class.forName("scala.tools.jline.TerminalFactory").
        getMethod("create").invoke(null).asInstanceOf[Terminal]
      terminal.getHeight()
    } catch {
      case _ :Throwable => try {
        type Terminal = { def getTerminalWidth() : Int; def getTerminalHeight() : Int; }
        val terminal = Class.forName("jline.Terminal").
          getMethod("getInstance").invoke(null).asInstanceOf[Terminal]
        terminal.getTerminalHeight()
      } catch {
        case _ :Throwable => 24
      }
    }
  }

  val newline = System.getProperty("line.separator")
}
