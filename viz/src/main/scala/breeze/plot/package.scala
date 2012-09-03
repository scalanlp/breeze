package breeze

import java.awt.{Color, Stroke, Paint}
import linalg.Matrix
import org.jfree.chart.renderer.xy.XYItemRenderer
import org.jfree.data.xy

/**
 * Basic 2-d plotting package. This API is very experimentable, I
 * reserve the right to change everything.
 * @author dlwh
 */
package object plot {
  /**
   * Plots the given y versus the given x with the given style.
   *
   * @param x X-coordinates, co-indexed with y (and indexed by keys of type K).
   * @param y Y-coordinates, co-indexed with x (and indexed by keys of type K).
   * @param style Matlab-like style spec of the series to plot.
   * @param name Name of the series to show in the legend.
   * @param labels Optional in-graph labels for some points.
   * @param tips Optional mouse-over tooltips for some points.
   */
  def plot[X,Y,V](x: X, y: Y, style : Char = '-', colorcode : String = null, name : String = null,
                  lines : Boolean = true, shapes : Boolean = false,
                  labels : (Int) => String = null.asInstanceOf[Int=>String],
                  tips : (Int) => String = null.asInstanceOf[Int=>String] )
                 (implicit xv: DomainFunction[X,Int,V],
                  yv: DomainFunction[Y, Int, V], vv: V=>Double):Series = new Series {
    require(xv.domain(x) == yv.domain(y), "Domains must match!")

    def getChartStuff(defaultName: (Int) => String, defaultColor: (Int) => Paint, defaultStroke: (Int) => Stroke): (xy.XYDataset, XYItemRenderer) = {

      type K = Int

      val dataset = XYDataset[Int](
        items = xv.domain(x),
        name = if(name == null) defaultName(0) else name,
        x = (k : K) => vv(xv(x,k)),
        y = (k : K) => vv(yv(y,k)),
        label = (k : K) => if (labels != null) labels(k) else null,
        tip = (k : K) => if (tips != null) tips(k) else null
      )


      // initialize the series renderer
      val renderer = new org.jfree.chart.renderer.xy.XYLineAndShapeRenderer()

      val color = if(colorcode != null) PaintScale.convertToColor(colorcode) else defaultColor(0)
      renderer.setSeriesPaint(0, color)
      renderer.setSeriesFillPaint(0, color)
      renderer.setSeriesOutlinePaint(0, color)
      renderer.setSeriesStroke(0, defaultStroke(0))
      //      renderer.setSeriesShape(0, defaultStroke(0))
      renderer.setSeriesOutlineStroke(0, defaultStroke(0))

      val tooltipGenerator = new org.jfree.chart.labels.XYToolTipGenerator() {
        override def generateToolTip(dataset : org.jfree.data.xy.XYDataset,
                                     series : Int, item : Int) : String = {
          dataset.asInstanceOf[XYDataset[_]].getTip(series, item)
        }
      }
      renderer.setSeriesToolTipGenerator(0, tooltipGenerator)

      val labelGenerator = new org.jfree.chart.labels.XYItemLabelGenerator() {
        override def generateLabel(dataset : org.jfree.data.xy.XYDataset,
                                   series : Int, item : Int) : String = {
          dataset.asInstanceOf[XYDataset[_]].getLabel(series, item)
        }
      }
      renderer.setSeriesItemLabelGenerator(0, labelGenerator)
      renderer.setSeriesItemLabelsVisible(0, labels != null)

      style match {
        case '-' =>
          // default line type
          renderer.setSeriesLinesVisible(0,lines)
          renderer.setSeriesShapesVisible(0,shapes)
        case '.' =>
          renderer.setSeriesLinesVisible(0,false)
          renderer.setSeriesShapesVisible(0,true)
          renderer.setSeriesShape(0,Plot.dot)
        case '+' =>
          renderer.setSeriesLinesVisible(0,false)
          renderer.setSeriesShapesVisible(0,true)
          renderer.setSeriesShape(0,Plot.plus)
        case _ =>
          throw new IllegalArgumentException("Expected style to be one of - . or +")
      }

      dataset -> renderer

    }
  }

  /**
    * Displays a scatter plot of x versus y, each point drawn at the given
    * size and mapped with the given color.
    *
    * Example usage: https://gist.github.com/1288473
    *
    * @param x The x coordinates to draw as provided by anything that can be seen as a Tensor1.
    * @param y The y coordinates to draw as provided by anything that can be seen as a Tensor1.
    * @param size The size of each circle (on the same scale as the domain axis)
    * @param c A partial function (e.g. a Map) from item ids to the color to draw the bubble.
    *   Missing colors are drawn with a hashed pattern.
    * @param labels Labels to draw next to each point.
    * @param tips Tooltips to show on mouseover for each point.
    * @param name Series name for legend
   */
  def scatter[X,V,Y,YV] (x : X, y : Y, size : Int=>Double,
                         colors : Int=>Paint = null.asInstanceOf[Int=>Paint],
                         labels : Int=>String = null.asInstanceOf[Int=>String],
                         tips : Int=>String = null.asInstanceOf[Int=>String],
                         name : String = null)
                        (implicit xv: DomainFunction[X,Int,V],
                         yv: DomainFunction[Y, Int, V], vv: V=>Double):Series = new Series {
    require(xv.domain(x) == yv.domain(y), "Domains must match!")


    /**
     * returns data needed by jfreechart.
     * @param defaultName series index => default name
     * @param defaultColor series index => default color
     * @param defaultStroke
     * @return
     */
    def getChartStuff(defaultName: (Int) => String, defaultColor: (Int) => Paint, defaultStroke: (Int) => Stroke): (xy.XYDataset, XYItemRenderer) = {

      val items = (xv.domain(x)).toIndexedSeq

      val paintScale = CategoricalPaintScale[K](colors)

      type K = Int
      // initialize dataset
      val dataset = XYZDataset(
        items = items,
        name = if (name == null) defaultName(0) else name,
        x = (k : K) => vv(xv(x,k)),
        y = (k : K) => vv(yv(y,k)),
        z = (k : K) => size(k),
        label = (k : K) => if (labels != null) labels(k) else null,
        tip = (k : K) => if (tips != null) tips(k) else null
      )

      // initialize the series renderer
      import org.jfree.chart.renderer.xy.XYBubbleRenderer
      val renderer = new XYBubbleRenderer(XYBubbleRenderer.SCALE_ON_DOMAIN_AXIS) {
        val stroke = new java.awt.BasicStroke(0f)
        override def getItemPaint(series : Int, item : Int) : java.awt.Paint =
          paintScale(items(item))
        override def getItemStroke(series : Int, item : Int) = stroke
      }

      val tooltipGenerator = new org.jfree.chart.labels.XYToolTipGenerator() {
        override def generateToolTip(dataset : org.jfree.data.xy.XYDataset, series : Int, item : Int) : String = {
          dataset.asInstanceOf[XYZDataset[_]].getTip(0, item)
        }
      }
      renderer.setSeriesToolTipGenerator(0, tooltipGenerator)

      val labelGenerator = new org.jfree.chart.labels.BubbleXYItemLabelGenerator() {
        override def generateLabel(dataset : org.jfree.data.xy.XYDataset, series : Int, item : Int) : String = {
          dataset.asInstanceOf[XYZDataset[_]].getLabel(0, item)
        }
      }
      renderer.setSeriesItemLabelGenerator(0, labelGenerator)
      renderer.setSeriesItemLabelsVisible(0, labels != null)

      dataset -> renderer
    }
  }

  /** Plots a histogram of the given data into the given number of bins */

  def hist[D,K,V](data : D, bins : HistogramBins = 10, name : String = null)
                 (implicit xv: DomainFunction[D,Int,V], vv: V=>Double):Series = new Series {
    val values = xv.domain(data).map(xv(data,_)).map(vv)
    val (min,max) = (values.min,values.max)
    val binner : StaticHistogramBins = bins match {
      case static : StaticHistogramBins => static
      case dynamic : DynamicHistogramBins =>
        dynamic(min, max)
    }

    val counts = new Array[Int](binner.splits.length + 1)
    for (value <- values) {
      counts(binner.bin(value)) += 1
    }

    val width = (binner.splits.iterator zip binner.splits.iterator.drop(1)).map(tup => tup._2 - tup._1).min

    def getChartStuff(defaultName: (Int) => String, defaultColor: (Int) => Paint, defaultStroke: (Int) => Stroke): (xy.XYDataset, XYItemRenderer) = {
      val dataset = new org.jfree.data.xy.XYBarDataset(
        XYDataset(
          name = if(name == null) defaultName(0) else name,
          items = IndexedSeq.range(0,counts.length),
          x = (i : Int) =>
            if (i == binner.splits.length) {
              binner.splits(i - 1) + width / 2.0
            } else {
              binner.splits(i) - width / 2.0
            },
          y = (i : Int) => counts(i),
          label = (i : Int) => null,
          tip = (i : Int) => null
        ), width)


      val renderer = new org.jfree.chart.renderer.xy.XYBarRenderer
      renderer.setSeriesPaint(0, defaultColor(0))
//      renderer.setSeriesShape(0, XYPlot.shape(series))
      renderer.setSeriesStroke(0, defaultStroke(0))
      renderer.setSeriesFillPaint(0, defaultColor(0))
      renderer.setSeriesOutlinePaint(0, defaultColor(0))
      renderer.setSeriesOutlineStroke(0, defaultStroke(0))
      renderer.setShadowVisible(false)
      renderer.setBarPainter(new org.jfree.chart.renderer.xy.StandardXYBarPainter())

      dataset -> renderer
    }
  }

  /**
   * Displays an image in the current figure, where each cell in the matrix
   * provides color for one square of the image.
   *
   * @param img A matrix containing the colors to plot
   * @param scale Scale used for converting matrix values to colors.
   * @param name Series name
   * @param offset Offset for indexing the top-left corner of the matrix
   * @param labels Labels for some subset of the data points
   * @param tips Tooltip popups for some subset of the data points
   */
  def image[M,V]
  (img : Matrix[Double],
   scale : GradientPaintScale[Double] = null,
   name : String = null,
   offset : (Int,Int) = (0,0),
   labels : PartialFunction[(Int,Int), String] = null.asInstanceOf[PartialFunction[(Int,Int), String]],
   tips : PartialFunction[(Int,Int), String] = null.asInstanceOf[PartialFunction[(Int,Int), String]]):Series = new Series {

    val mt = img

    val (minx,maxx) = (0,img.cols)
    val (miny,maxy) = (0,img.rows)

    val items = img.keysIterator.toIndexedSeq

    def getChartStuff(defaultName: (Int) => String, defaultColor: (Int) => Paint, defaultStroke: (Int) => Stroke): (xy.XYDataset, XYItemRenderer) = {
      // initialize dataset
      val dataset = XYZDataset(
        items = items,
        name = if (name == null) defaultName(0) else name,
        x = (k : (Int,Int)) => k._2 + offset._2,
        y = (k : (Int,Int)) => k._1 + offset._1,
        z = (k : (Int,Int)) => mt(k._1,k._2),
        label = (k : (Int,Int)) => if (labels != null && labels.isDefinedAt(k)) labels(k) else null,
        tip = (k : (Int,Int)) => if (tips != null && tips.isDefinedAt(k)) tips(k) else null
      )

      // initialize renderer
      import org.jfree.chart.renderer.xy.XYBlockRenderer
      val renderer = new XYBlockRenderer()
      renderer.setSeriesPaint(0, defaultColor(0))
//      renderer.setSeriesShape(0, XYPlot.shape(series))
      renderer.setSeriesStroke(0, defaultStroke(0))
      renderer.setSeriesFillPaint(0, defaultColor(0))
      renderer.setSeriesOutlinePaint(0, defaultColor(0))
      renderer.setSeriesOutlineStroke(0, defaultStroke(0))

      val tooltipGenerator = new org.jfree.chart.labels.XYToolTipGenerator() {
        override def generateToolTip(dataset : org.jfree.data.xy.XYDataset, series : Int, item : Int) : String = {
          dataset.asInstanceOf[XYZDataset[_]].getTip(series, item)
        }
      }
      renderer.setSeriesToolTipGenerator(0, tooltipGenerator)

      val labelGenerator = new org.jfree.chart.labels.XYItemLabelGenerator() {
        override def generateLabel(dataset : org.jfree.data.xy.XYDataset, series : Int, item : Int) : String = {
          dataset.asInstanceOf[XYZDataset[_]].getLabel(series, item)
        }
      }
      renderer.setSeriesItemLabelGenerator(0, labelGenerator)
      renderer.setSeriesItemLabelsVisible(0, labels != null)

      val staticScale = {
        if (scale == null)
          GradientPaintScaleFactory[Double]().apply(mt.valuesIterator.toList).asInstanceOf[GradientPaintScale[Double]]
        else
          scale
      }

      val paintScale = new org.jfree.chart.renderer.PaintScale {
        override def getLowerBound = staticScale.lower
        override def getUpperBound = staticScale.upper
        override def getPaint(value : Double) =
          staticScale(value)
      }

      renderer.setPaintScale(paintScale)
      renderer.setBlockAnchor(org.jfree.ui.RectangleAnchor.BOTTOM_LEFT)
      renderer.setBlockWidth(1)
      renderer.setBlockHeight(1)

      dataset -> renderer
    }


  }
}
