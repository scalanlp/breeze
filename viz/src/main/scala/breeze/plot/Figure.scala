package breeze.plot

import collection.mutable.ArrayBuffer
import javax.swing.{SwingUtilities, WindowConstants, JFrame, JPanel}
import java.util.concurrent.atomic.AtomicInteger
import java.awt.{Color, Paint, Graphics2D}
import org.jfree.chart.axis.{NumberTickUnit, TickUnits}
import org.jfree.chart.plot.DefaultDrawingSupplier
import breeze.plot.Plot.Listener

/**
 *
 * @author dlwh, dramage
 */
class Figure(name: String, private var rows_ : Int  = 1, private var cols_ :Int = 1) {

  protected val plots = ArrayBuffer[Option[Plot]]()

  private var width_ = 600
  private var height_ = 400

  /**Selects the given subplot.  */
  def subplot(i: Int) = selectPlot(i)

  def clearPlot(i: Int) {
    if(i < plots.length) plots(i) = None
    refresh()
  }


  /** Selects the given subplot.  Note that select is 0-based, and is therefore incompatible with matlab and Scalala */
  def subplot(rows:Int,cols:Int,select:Int) : Plot  = {
    this.rows = rows
    this.cols = cols

    refresh()
    selectPlot(select)
  }

  /** Width of figure on screen (or in image) */
  def width = width_
  def width_=(newwidth : Int) : Unit  = {
    width_ = newwidth
    refresh()
  }

  /** Height of figure on screen (or in image) */
  def height = height_
  def height_=(newheight : Int) : Unit  = {
    height_ = newheight
    refresh()
  }

  /** How many rows of plots are in the figure */
  def rows = rows_
  def rows_=(newrows : Int) : Unit  = {
    rows_ = newrows
    refresh()
  }

  /** How many cols of plots are in the figure */
  def cols = cols_
  def cols_=(newcols : Int) : Unit  = {
    cols_ = newcols
    refresh()
  }


  /** Visibility state of the plot */
  private var visible_ = true
  def visible = visible_
  def visible_=(newvis : Boolean) : Unit = {
    visible_ = newvis
    frame.setVisible(visible_)
  }


  /** JPanel holding for drawing subplots in this figure. */
  private val contents = {
    val _c = new JPanel()
    _c.setSize(width_, height_)
    _c
  }


  /** The Swing frame for this plot */
  private lazy val frame : JFrame = {
    val f = new JFrame(name)
    f.setSize(width_, height_)
    f.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    f.setLayout(new java.awt.BorderLayout())
    f.add(contents, java.awt.BorderLayout.CENTER)

    // we use visible_ to avoid an infinite loop
    f.setVisible(visible_)

    f
  }

  /** Clears the current figure */
  def clear() {
    contents.removeAll()
    plots.clear()
    rows = 1
    cols = 1
    plots += None
    refresh()
  }

  /** Redraws the figure */
  def refresh()  = {
    while (plots.length < rows * cols) {
      plots += None
    }
    while (plots.length > rows * cols) {
      plots.remove(plots.length-1)
    }

    SwingUtilities.invokeLater(new Runnable {
      def run() {
        contents.removeAll()
        contents.setSize(width_, height_)
        contents.setLayout(new java.awt.GridLayout(rows,cols))
        for (plot <- plots) {
          contents.add(plot match { case Some(plot) => plot.panel; case None => new JPanel() })
        }
        frame.setSize(width_, height_)
        frame.setVisible(visible)
      }
    })

    frame.repaint()
  }

  def drawPlots(g2d : Graphics2D) {
    val plotWidth  = contents.getWidth / cols
    val plotHeight = contents.getHeight / rows
    var px = 0; var py = 0
    for (opt <- plots) {
      opt match {
        case Some(plot) =>
          plot.chart.draw(g2d, new java.awt.Rectangle(px*plotWidth, py*plotHeight, plotWidth, plotHeight))
        case None => {}
      }
      px = (px +1)%cols
      if(px == 0) py = (py + 1)%rows
    }
  }

  /** Saves the current figure at the requested dpi to the given filename. */
  def saveas(filename : String, dpi : Int = 72) {
    // make sure figure is visible or saved image will come up empty
    refresh()

    ExportGraphics.writeFile(
      new java.io.File(filename),
      draw = drawPlots _,
      width = contents.getWidth,
      height = contents.getHeight,
      dpi = dpi)
  }

  private def selectPlot(i: Int)  = {
    var j = plots.length
    while(j < i) {
      plots += None
      j += 1
    }
    val plot = if(i >= plots.length) {
      plots += Some(new Plot)
      plots.last.get
    } else {
      if(plots(i) == None) {
        plots(i) = Some(new Plot)
      }
      plots(i).get
    }

    plot listen new Plot.Listener {
      def refresh(pl: Plot) {
        Figure.this.refresh()
      }
    }

    plot

  }

}

object Figure {

  def apply(name: String):Figure = new Figure(name)
  def apply():Figure = apply("Figure " + figureNumber.getAndIncrement)

  private val figureNumber = new AtomicInteger(0)
}


