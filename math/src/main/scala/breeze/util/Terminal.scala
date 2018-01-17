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
//  lazy val (terminalWidth: Int, terminalHeight : Int) = {
//    try {
//      val terminal = Class.forName("scala.tools.jline.TerminalFactory").
//        getMethod("create").invoke(null)
//      val width = terminal.getClass.getMethod("getWidth").invoke(terminal).asInstanceOf[java.lang.Integer].intValue()
//      val height = terminal.getClass.getMethod("getHeight").invoke(terminal).asInstanceOf[java.lang.Integer].intValue()
//      width -> height
//    } catch {
//      case _ : Throwable => 80
//    }
//  }

//    val (terminalWidth: Int, terminalHeight : Int) = (80, 80)
  val terminalWidth: Int = 80
  val terminalHeight: Int = 80

  val newline = "\n" //System.getProperty("line.separator")
}
