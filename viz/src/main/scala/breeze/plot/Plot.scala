package breeze.plot


import org.jfree.chart.JFreeChart
import org.jfree.chart.plot.{CrosshairState, PlotRenderingInfo, DefaultDrawingSupplier}
import org.jfree.chart.axis._
import java.awt._
import collection.mutable.ArrayBuffer
import collection.mutable
import org.jfree.chart.renderer.xy.{XYItemRendererState, XYItemRenderer, AbstractXYItemRenderer}
import java.awt.geom.Rectangle2D
import org.jfree.data.xy
import java.lang
import scala.List

/**
 * Maintains a set of series (or more strictly, the data from those series)
 * and the necessary JFreeChart stuff to plot them.
 *
 * @author dlwh
 */
class Plot() {
  private val datasets = new ArrayBuffer[xy.XYDataset]
  private val renderers = new ArrayBuffer[XYItemRenderer]
  private var series = 0
  private val listeners = new mutable.WeakHashMap[Plot.Listener,Unit]()

  def +=(pl: Series) = {
    val (d,r) = pl.getChartStuff( {i => "Series " + (series + i)}, {i => Plot.fillPaint(series +i)}, {i => Plot.stroke(series + i)})
    datasets += d
    renderers += r
    series += d.getSeriesCount
    refresh()
    this
  }

  def ++=(pl: TraversableOnce[Series]) = {
    pl foreach (this +=  _)
    refresh()
    this
  }

  def refresh() {
    var series = 0
    for( (d,r) <- datasets zip renderers; s <- 0 until d.getSeriesCount) {
      plot.setDataset(series, d)
      plot.setRenderer(series, r)
      series +=1
    }

    listeners foreach { _._1.refresh(this) }
  }

  // Sigh, I hate the listener pattern
  def listen(l: Plot.Listener) {
    listeners += (l -> ())
  }

  def unlisten(l: Plot.Listener) {
    listeners -= (l)
  }

  def xlabel = xaxis.getLabel
  def xlabel_=(label: String) {
    xaxis.setLabel(label)
  }

  def ylabel = yaxis.getLabel
  def ylabel_=(label: String) {
    yaxis.setLabel(label)
  }

  def xlim_=(lowerUpper: (Double,Double)) {
    xlim(lowerUpper._1, lowerUpper._2)
  }
  def xlim = plot.getDomainAxis.getLowerBound -> plot.getDomainAxis.getUpperBound()
  /** Sets the lower and upper bounds of the current plot. */
  def xlim(lower: Double, upper: Double) {
    plot.getDomainAxis.setLowerBound(lower)
    plot.getDomainAxis.setUpperBound(upper)
  }

  def ylim_=(lowerUpper: (Double,Double)) {
    ylim(lowerUpper._1, lowerUpper._2)
  }
  def ylim = plot.getRangeAxis.getLowerBound -> plot.getRangeAxis.getUpperBound()
  def ylim(lower: Double, upper: Double) {
    plot.getRangeAxis.setLowerBound(lower)
    plot.getRangeAxis.setUpperBound(upper)
  }

  def xaxis = _xaxis
  def yaxis = _yaxis

  private var _xaxis : NumberAxis = new NumberAxis(null)
  private var _yaxis : NumberAxis = new NumberAxis(null)

  def logScaleX = xaxis.isInstanceOf[LogarithmicAxis]
  def logScaleY = yaxis.isInstanceOf[LogarithmicAxis]

  def logScaleX_=(value: Boolean) {
    if(value != logScaleX) {
      // TODO this is such a pain. There has to be a better way.
      val oldAxis = _xaxis
      _xaxis = if(value) new LogarithmicAxis(xlabel) else new NumberAxis(xlabel)
      plot.setDomainAxis(_xaxis)
      xlim = oldAxis.getLowerBound -> oldAxis.getUpperBound
      _xaxis.setStandardTickUnits(oldAxis.getStandardTickUnits)
      _xaxis.setAutoRangeIncludesZero(oldAxis.getAutoRangeIncludesZero)
    }
  }

  def logScaleY_=(value: Boolean) {
    if(value != logScaleY) {
      // TODO this is such a pain. There has to be a better way.
      val oldAxis = _yaxis
      _yaxis = if(value) new LogarithmicAxis(ylabel) else new NumberAxis(ylabel)
      plot.setRangeAxis(_yaxis)
      ylim = oldAxis.getLowerBound -> oldAxis.getUpperBound
      _yaxis.setStandardTickUnits(oldAxis.getStandardTickUnits)
      _yaxis.setAutoRangeIncludesZero(oldAxis.getAutoRangeIncludesZero)
    }
  }



  /** The plot title */
  def title_=(str : String) {
    chart.setTitle(str)
  }
  def title : String = chart.getTitle.getText

  // Save the default tick units.
  private val xaxisDefaultTickUnits = xaxis.getStandardTickUnits
  private val yaxisDefaultTickUnits = yaxis.getStandardTickUnits

  def setXAxisIntegerTickUnits() {
    xaxis.setStandardTickUnits(Plot.integerTickUnits)
  }
  def setYAxisIntegerTickUnits() {
    yaxis.setStandardTickUnits(Plot.integerTickUnits)
  }
  def setXAxisDecimalTickUnits() {
    xaxis.setStandardTickUnits(xaxisDefaultTickUnits)
  }
  def setYAxisDecimalTickUnits() {
    yaxis.setStandardTickUnits(yaxisDefaultTickUnits)
  }

  // set integer tick units by default
  Array(xaxis,yaxis) foreach (axis => {
    axis.setAutoRangeIncludesZero(false)
    axis.setStandardTickUnits(Plot.integerTickUnits)
  })

  /** The JFreeChart plot object. */
  lazy val plot = {
    val rv = new org.jfree.chart.plot.XYPlot()
    rv.setDomainAxis(xaxis)
    rv.setRangeAxis(yaxis)

    rv.setDrawingSupplier(new DefaultDrawingSupplier(
      Plot.paints,
      Plot.fillPaints,
      Plot.outlinePaints,
      Plot.strokes,
      Plot.outlineStrokes,
      Plot.shapes))

    rv
  }

  /** If we show a legend */
  private var _legend : Boolean = false
  def legend_=(show : Boolean) : Unit = {
    chart.removeLegend()
    if (show) {
      import org.jfree.chart.title._
      import org.jfree.ui._
      import org.jfree.chart.block._
      import java.awt.Color
      val legend = new LegendTitle(this.plot)
      legend.setMargin(new RectangleInsets(1.0, 1.0, 1.0, 1.0))
      legend.setFrame(new LineBorder())
      legend.setBackgroundPaint(Color.WHITE)
      legend.setPosition(RectangleEdge.BOTTOM)
      chart.addLegend(legend)
    }
    refresh()
    _legend = show
  }
  def legend : Boolean = _legend


  /** The JFreeChart for this plot */
  lazy val chart = {
    val rv = new JFreeChart(null, JFreeChart.DEFAULT_TITLE_FONT, plot, false)
    rv.setBackgroundPaint(Plot.transparent)
    rv.setPadding(new org.jfree.ui.RectangleInsets(5,0,0,0))
    rv
  }

  /** The ChartPanel for this plot */
  lazy val panel =
    new org.jfree.chart.ChartPanel(chart)

}

object Plot {

  trait Listener {
    def refresh(pl: Plot)
  }

  val integerTickUnits = {
    val units = new TickUnits()
    val df = new java.text.DecimalFormat("0")
    for (b <- List(1,2,5); e <- List(0,1,2,3,4,5,6,7,8)) {
      units.add(new NumberTickUnit(b * math.pow(10,e).toInt, df))
    }
    units
  }


  // color cycle ignoring bright colors
  val paints : Array[Paint] = PaintScale.Category20.values.asInstanceOf[Array[Paint]]

  def paint(series : Int) =
    paints(series % paints.length)

  val shapes = DefaultDrawingSupplier.DEFAULT_SHAPE_SEQUENCE
  def shape(series : Int) =
    shapes(series % shapes.length)

  val strokes = DefaultDrawingSupplier.DEFAULT_STROKE_SEQUENCE
  def stroke(series : Int) =
    strokes(series % strokes.length)

  val fillPaints = paints; // DefaultDrawingSupplier.DEFAULT_FILL_PAINT_SEQUENCE
  def fillPaint(series : Int) =
    fillPaints(series % fillPaints.length)

  val outlinePaints = paints; // DefaultDrawingSupplier.DEFAULT_OUTLINE_PAINT_SEQUENCE
  def outlinePaint(series : Int) =
    outlinePaints(series % outlinePaints.length)

  val outlineStrokes = DefaultDrawingSupplier.DEFAULT_OUTLINE_STROKE_SEQUENCE
  def outlineStroke(series : Int) =
    outlineStrokes(series % outlineStrokes.length)

  val transparent = new Color(255,255,255,0)

  //
  // shapes
  //

  val dot =
    new java.awt.geom.Ellipse2D.Double(-1,-1,2,2)

  val plus = {
    val shape = new java.awt.geom.GeneralPath()
    shape.moveTo(-3,0)
    shape.lineTo(3,0)
    shape.moveTo(0,-3)
    shape.lineTo(0,3)
    shape
  }

  import org.jfree.data.xy
  class DelegatingDataset extends xy.AbstractXYZDataset {
    private val datasets = ArrayBuffer[xy.XYDataset]()
    // datasets.length + 1, last index is seriesDelegates.length
    // each index is the offset for the dataset.
    private val datasetSeriesOffsets = ArrayBuffer[Int]()
    datasetSeriesOffsets += 0
    private val seriesDelegates = ArrayBuffer[Int]()

    private def delegate[A](series: Int)(f: (xy.XYDataset,Int)=>A) = {
      f(datasets(seriesDelegates(series)), series - datasetSeriesOffsets(seriesDelegates(series)))
    }

    def clear() {
      datasets.clear()
      datasetSeriesOffsets.clear()
      datasetSeriesOffsets += 0
      seriesDelegates.clear()
    }


    def +=(d: xy.XYDataset) {
      datasets += d
      for(i <- 0 until d.getSeriesCount) {
        seriesDelegates += (datasets.size - 1)
      }
      datasetSeriesOffsets += seriesDelegates.length
    }

    def getItemCount(series: Int): Int = delegate(series)(_ getItemCount _ )

    def getX(p1: Int, p2: Int): Number = { delegate(p1)( _.getX(_,p2))}
    def getY(p1: Int, p2: Int): Number = delegate(p1)( _.getY(_,p2))
    def getZ(p1: Int, p2: Int): Number = delegate(p1)( _.asInstanceOf[xy.XYZDataset].getZ(_,p2))

    def getSeriesCount: Int = seriesDelegates.length

    def getSeriesKey(series: Int): Comparable[_] = {
      val name = delegate(series)(_  getSeriesKey _)
      if(name == null) "Series " + series
      else name
    }
  }

  class DelegatingRenderer extends AbstractXYItemRenderer {
    import org.jfree.chart.renderer.xy
    private val renderers = ArrayBuffer[xy.XYItemRenderer]()
      // datasets.length + 1, last index is seriesDelegates.length
      // each index is the offset for the dataset.
      private val datasetSeriesOffsets = ArrayBuffer[Int]()
      datasetSeriesOffsets += 0
      private val seriesDelegates = ArrayBuffer[Int]()
      private val autopaint = ArrayBuffer[Boolean]()
    private val autostroke = ArrayBuffer[Boolean]()

      private def delegate[A](series: Int)(f: (xy.XYItemRenderer,Int)=>A) = {
        f(renderers(seriesDelegates(series)), series - datasetSeriesOffsets(seriesDelegates(series)))
      }

      def clear() {
        renderers.clear()
        datasetSeriesOffsets.clear()
        datasetSeriesOffsets += 0
        seriesDelegates.clear()
      }


      def +=(d: xy.XYItemRenderer, numSeries:Int, autocolor: Boolean, autostroke: Boolean) {
        renderers += d
        for(i <- 0 until numSeries) {
          seriesDelegates += (renderers.size - 1)
          autopaint += autocolor
          this.autostroke += autostroke
        }
        datasetSeriesOffsets += seriesDelegates.length
      }

    def drawItem(p1: Graphics2D,
                 p2: XYItemRendererState,
                 p3: Rectangle2D,
                 p4: PlotRenderingInfo,
                 p5: org.jfree.chart.plot.XYPlot,
                 p6: ValueAxis, p7: ValueAxis,
                 p8: org.jfree.data.xy.XYDataset, series: Int, item: Int, p11: CrosshairState, p12: Int) {
      delegate(series)(_.drawItem(p1,p2,p3,p4,p5,p6,p7,p8,_,item,p11,p12))


    }

    override def getItemVisible(series: Int, item: Int): Boolean = {
      delegate(series)(_.getItemVisible(_,item))
    }

    override def isSeriesVisible(series: Int): Boolean = delegate(series)(_.isSeriesVisible(_))

    override def getSeriesVisible(series: Int): lang.Boolean = delegate(series)(_.isSeriesVisible(_))

    override def setSeriesVisible(series: Int, visible: lang.Boolean) {
      delegate(series)(_.setSeriesVisible(_,visible))
    }

    override def setSeriesVisible(series: Int, visible: lang.Boolean, notify: Boolean) {
      delegate(series)(_.setSeriesVisible(_,visible, notify))
    }

    override def isSeriesVisibleInLegend(series: Int): Boolean = {
      delegate(series)(_.isSeriesVisibleInLegend(_))
    }

    override def getItemPaint(series: Int, column: Int): Paint = {
      if(autopaint(series)) Plot.paint(series)
      else delegate(series)(_.getItemPaint(_,column))
    }

    override def getSeriesVisibleInLegend(series: Int): lang.Boolean = {
      delegate(series)(_.getSeriesVisibleInLegend(_))
    }

    override def setSeriesVisibleInLegend(series: Int, visible: lang.Boolean) {
      delegate(series)(_.setSeriesVisibleInLegend(_, visible))
    }

    override def setSeriesVisibleInLegend(series: Int, visible: lang.Boolean, notify: Boolean) {
      delegate(series)(_.setSeriesVisibleInLegend(_, visible, notify))
    }

    override def getSeriesPaint(series: Int): Paint = {
      if(autopaint(series)) Plot.paint(series)
      else delegate(series)(_.getSeriesPaint(_))
    }

    override def setSeriesPaint(series: Int, paint: Paint) {
      delegate(series)(_.setSeriesPaint(_, paint))
      autopaint(series) = false
    }

    override def getItemOutlinePaint(series: Int, column: Int): Paint = {
      if(autopaint(series)) Plot.paint(series)
      else delegate(series)(_.getItemOutlinePaint(_,column))
    }

    override def setSeriesOutlinePaint(series: Int, paint: Paint) {
      delegate(series)(_.setSeriesOutlinePaint(_, paint))
    }

    override def getSeriesOutlinePaint(series: Int): Paint = {
      if(autopaint(series)) Plot.paint(series)
      else delegate(series)(_.getSeriesOutlinePaint(_))
    }

    override def getItemStroke(series: Int, column: Int): Stroke = {
      if(autostroke(series)) Plot.stroke(series)
       else delegate(series)(_.getItemStroke(_,column))

      //    renderer.setSeriesStroke(0, Plot.stroke(series))
      //    renderer.setSeriesShape(0, Plot.shape(series))
      //    renderer.setSeriesOutlineStroke(0, Plot.outlineStroke(series))
    }

    override def getSeriesStroke(series: Int): Stroke = {
      if(autostroke(series)) Plot.stroke(series)
      else  delegate(series)(_.getSeriesStroke(_))
    }

    override def setSeriesStroke(series: Int, stroke: Stroke) {
      delegate(series)(_.setSeriesStroke(_, stroke))
    }

    override def getItemOutlineStroke(series: Int, column: Int): Stroke = {
      if(autostroke(series)) Plot.outlineStroke(series)
      else   delegate(series)(_.getItemOutlineStroke(_,column))
    }

    override def setSeriesOutlineStroke(series: Int, stroke: Stroke) {
      delegate(series)(_.setSeriesOutlineStroke(_,stroke))
    }

    override def getSeriesOutlineStroke(series: Int): Stroke = {
      if(autostroke(series)) Plot.outlineStroke(series)
      else  delegate(series)(_.getSeriesOutlineStroke(_))
    }

    override def getSeriesShape(series: Int): Shape = {
      if(autostroke(series)) Plot.shape(series)
      else   delegate(series)(_.getSeriesShape(_))
    }

    override def setSeriesShape(series: Int, shape: Shape) {
      delegate(series)(_.setSeriesShape(_,shape))
    }

    override def getItemShape(series: Int, column: Int): Shape = {
      if(autostroke(series)) Plot.shape(series)
      else  delegate(series)(_.getItemShape(_,column))
    }

    override def isItemLabelVisible(series: Int, column: Int): Boolean = {
      delegate(series)(_.isItemLabelVisible(_,column))
    }

    override def isSeriesItemLabelsVisible(series: Int): Boolean = {
      delegate(series)(_.isSeriesItemLabelsVisible(_))
    }




  }


}

