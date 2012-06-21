package breeze.util

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
      case _ => try {
        type Terminal = { def getTerminalWidth() : Int; def getTerminalHeight() : Int; }
        val terminal = Class.forName("jline.Terminal").
          getMethod("getInstance").invoke(null).asInstanceOf[Terminal]
        terminal.getTerminalWidth()
      } catch {
        case _ => 80
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
      case _ => try {
        type Terminal = { def getTerminalWidth() : Int; def getTerminalHeight() : Int; }
        val terminal = Class.forName("jline.Terminal").
          getMethod("getInstance").invoke(null).asInstanceOf[Terminal]
        terminal.getTerminalHeight()
      } catch {
        case _ => 24
      }
    }
  }
}
