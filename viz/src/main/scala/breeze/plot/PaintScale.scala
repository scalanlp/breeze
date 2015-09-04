package breeze.plot

import java.awt.{TexturePaint, Color, Paint}
import java.awt.image.BufferedImage
import java.awt.geom.Rectangle2D

/**
 * Maps items of type T to a well defined Paint (usually a color).
 *
 * An implicit conversion exists to make a singleton PaintScaleFactory from
 * a PaintScale instance, which means that PaintScales can be provided
 * directly whenever a PaintScaleFactory is required.
 *
 * @author dramage
 */
sealed trait PaintScale[T] extends (T => Paint)

/**
 * A simple numeric paint scale for mapping a number within a range to a
 * corresponding element of a pre-computed color gradient.  Colors from the
 * given gradient array are used linearly to represent values between
 * lower and upper.
 *
 * @author dramage
 */
case class GradientPaintScale[T]
(lower : T, upper : T, gradient : Array[Color] = PaintScale.WhiteToBlack)
(implicit view : T=>Double)
extends PaintScale[T] {
  def apply(value : T) : Paint = {
    if (view(value).isNaN) {
      PaintScale.nanPaint
    } else {
      val index = gradient.length * (value - lower) / (upper - lower)
      gradient(math.min(gradient.length-1, math.max(0, index.toInt)))
    }
  }
}

/**
 * Maps items to colors using the given partial function.  If no color
 * is provided for the given item, then returns PaintScale.nanPaint.
 *
 * @author dramage
 */
case class CategoricalPaintScale[T]
(categories : Function1[T,Paint])
extends PaintScale[T] {
  def apply(value : T) : Paint = {
    if (categories == null) {
      PaintScale.nanPaint
    } else {
      categories(value)
    }
  }

  private def ida(v: T) = categories match {
    case f: PartialFunction[T,Paint] => f.isDefinedAt(v)
    case _ => true
  }
}

object PaintScale {

  /**
   * Convert a color description string into a color suitable for plotting.
   * @param colorcode A string that is a single character (like "k"), a name
   * (like "black"), "r,g,b" or, "[r,g,b]"
   *
   * @author Patryk Laurent
   */
  def convertToColor(colorcode:String) : java.awt.Color = {
      val rgbcsv =  "(.*),(.*),(.*)".r
      colorcode.toLowerCase.replace(" ", "").replace("[","").replace("]", "") match {
          case "y" | "yellow"  => yellow
          case "m" | "magenta" => magenta
          case "c" | "cyan"    => cyan
          case "r" | "red"     => red
          case "g" | "green"   => green
          case "b" | "blue"    => blue
          case "w" | "white"   => white
          case "k" | "black"   => black
          case rgbcsv(r,g,b)   => new java.awt.Color(r.toInt,g.toInt,b.toInt)
          case uninterpretable:String => throw new IllegalArgumentException(
            "Expected color code to be either y m c r g b w k OR R,G,B or " +
            "[R,G,B] where R,G,B are numbers such that 0<=R,G,B<=255, but got '" +
            uninterpretable + "' instead.")
      }
  }

  /** Creates a GradientPaintScale automatically for the given range. */
  implicit def gradientTuple[T](vLowerUpper : (T,T))(implicit view : T=>Double)
  : GradientPaintScale[T] =
    GradientPaintScale[T](vLowerUpper._1, vLowerUpper._2)

  /** Creates a CategoricalPaintScale from the provided partial function. */
  implicit def literalColorMap[T](map : PartialFunction[T,Paint])
  : CategoricalPaintScale[T] =
    CategoricalPaintScale[T](map)


  //
  // Default colors and patterns.
  //

  /** For painting NaN. */
  val nanPaint = {
    val img = new BufferedImage(5,5,BufferedImage.TYPE_INT_ARGB)

    val gfx = img.getGraphics
    gfx.setColor(Color.gray)
    gfx.drawLine(0,0,4,4)
    gfx.dispose()

    new TexturePaint(img, new Rectangle2D.Double(0,0,5,5))
  }

  /** The Category10 palette from Protovis http://vis.stanford.edu/protovis/docs/color.html */
  object Category10 {
    val values : Array[Color] = Array(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
      "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
    ).map(Color.decode)

    val blue = values(0)
    val orange = values(1)
    val green = values(2)
    val red = values(3)
    val purple = values(4)
    val brown = values(5)
    val magenta = values(6)
    val gray = values(7)
    val gold = values(8)
    val teal = values(9)

    def apply(i : Int) = values(i)
  }

  /** The Category20 palette from Protovis http://vis.stanford.edu/protovis/docs/color.html */
  object Category20 {
    val values : Array[Color] = Category10.values ++ Array(
      "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5",
      "#c49c94", "#f7b6d2", "#c7c7c7", "#dbdb8d", "#9edae5"
    ).map(Color.decode)

    val lightblue = values(10)
    val lightorange = values(11)
    val lightgreen = values(12)
    val lightred = values(13)
    val lightpurple = values(14)
    val lightbrown = values(15)
    val lightmagenta = values(16)
    val lightgray = values(17)
    val lightgold = values(18)
    val lightteal = values(19)

    def apply(i : Int) = values(i)
  }

  //
  // A large pallete of color literals from ProtoVis
  //
  val aliceblue = Color.decode( "#f0f8ff")
  val antiquewhite = Color.decode( "#faebd7")
  val aqua = Color.decode( "#00ffff")
  val aquamarine = Color.decode( "#7fffd4")
  val azure = Color.decode( "#f0ffff")
  val beige = Color.decode( "#f5f5dc")
  val bisque = Color.decode( "#ffe4c4")
  val black = Color.decode( "#000000")
  val blanchedalmond = Color.decode( "#ffebcd")
  val blue = Color.decode( "#0000ff")
  val blueviolet = Color.decode( "#8a2be2")
  val brown = Color.decode( "#a52a2a")
  val burlywood = Color.decode( "#deb887")
  val cadetblue = Color.decode( "#5f9ea0")
  val chartreuse = Color.decode( "#7fff00")
  val chocolate = Color.decode( "#d2691e")
  val coral = Color.decode( "#ff7f50")
  val cornflowerblue = Color.decode( "#6495ed")
  val cornsilk = Color.decode( "#fff8dc")
  val crimson = Color.decode( "#dc143c")
  val cyan = Color.decode( "#00ffff")
  val darkblue = Color.decode( "#00008b")
  val darkcyan = Color.decode( "#008b8b")
  val darkgoldenrod = Color.decode( "#b8860b")
  val darkgray = Color.decode( "#a9a9a9")
  val darkgreen = Color.decode( "#006400")
  val darkgrey = Color.decode( "#a9a9a9")
  val darkkhaki = Color.decode( "#bdb76b")
  val darkmagenta = Color.decode( "#8b008b")
  val darkolivegreen = Color.decode( "#556b2f")
  val darkorange = Color.decode( "#ff8c00")
  val darkorchid = Color.decode( "#9932cc")
  val darkred = Color.decode( "#8b0000")
  val darksalmon = Color.decode( "#e9967a")
  val darkseagreen = Color.decode( "#8fbc8f")
  val darkslateblue = Color.decode( "#483d8b")
  val darkslategray = Color.decode( "#2f4f4f")
  val darkslategrey = Color.decode( "#2f4f4f")
  val darkturquoise = Color.decode( "#00ced1")
  val darkviolet = Color.decode( "#9400d3")
  val deeppink = Color.decode( "#ff1493")
  val deepskyblue = Color.decode( "#00bfff")
  val dimgray = Color.decode( "#696969")
  val dimgrey = Color.decode( "#696969")
  val dodgerblue = Color.decode( "#1e90ff")
  val firebrick = Color.decode( "#b22222")
  val floralwhite = Color.decode( "#fffaf0")
  val forestgreen = Color.decode( "#228b22")
  val fuchsia = Color.decode( "#ff00ff")
  val gainsboro = Color.decode( "#dcdcdc")
  val ghostwhite = Color.decode( "#f8f8ff")
  val gold = Color.decode( "#ffd700")
  val goldenrod = Color.decode( "#daa520")
  val gray = Color.decode( "#808080")
  val green = Color.decode( "#008000")
  val greenyellow = Color.decode( "#adff2f")
  val grey = Color.decode( "#808080")
  val honeydew = Color.decode( "#f0fff0")
  val hotpink = Color.decode( "#ff69b4")
  val indianred = Color.decode( "#cd5c5c")
  val indigo = Color.decode( "#4b0082")
  val ivory = Color.decode( "#fffff0")
  val khaki = Color.decode( "#f0e68c")
  val lavender = Color.decode( "#e6e6fa")
  val lavenderblush = Color.decode( "#fff0f5")
  val lawngreen = Color.decode( "#7cfc00")
  val lemonchiffon = Color.decode( "#fffacd")
  val lightblue = Color.decode( "#add8e6")
  val lightcoral = Color.decode( "#f08080")
  val lightcyan = Color.decode( "#e0ffff")
  val lightgoldenrodyellow = Color.decode( "#fafad2")
  val lightgray = Color.decode( "#d3d3d3")
  val lightgreen = Color.decode( "#90ee90")
  val lightgrey = Color.decode( "#d3d3d3")
  val lightpink = Color.decode( "#ffb6c1")
  val lightsalmon = Color.decode( "#ffa07a")
  val lightseagreen = Color.decode( "#20b2aa")
  val lightskyblue = Color.decode( "#87cefa")
  val lightslategray = Color.decode( "#778899")
  val lightslategrey = Color.decode( "#778899")
  val lightsteelblue = Color.decode( "#b0c4de")
  val lightyellow = Color.decode( "#ffffe0")
  val lime = Color.decode( "#00ff00")
  val limegreen = Color.decode( "#32cd32")
  val linen = Color.decode( "#faf0e6")
  val magenta = Color.decode( "#ff00ff")
  val maroon = Color.decode( "#800000")
  val mediumaquamarine = Color.decode( "#66cdaa")
  val mediumblue = Color.decode( "#0000cd")
  val mediumorchid = Color.decode( "#ba55d3")
  val mediumpurple = Color.decode( "#9370db")
  val mediumseagreen = Color.decode( "#3cb371")
  val mediumslateblue = Color.decode( "#7b68ee")
  val mediumspringgreen = Color.decode( "#00fa9a")
  val mediumturquoise = Color.decode( "#48d1cc")
  val mediumvioletred = Color.decode( "#c71585")
  val midnightblue = Color.decode( "#191970")
  val mintcream = Color.decode( "#f5fffa")
  val mistyrose = Color.decode( "#ffe4e1")
  val moccasin = Color.decode( "#ffe4b5")
  val navajowhite = Color.decode( "#ffdead")
  val navy = Color.decode( "#000080")
  val oldlace = Color.decode( "#fdf5e6")
  val olive = Color.decode( "#808000")
  val olivedrab = Color.decode( "#6b8e23")
  val orange = Color.decode( "#ffa500")
  val orangered = Color.decode( "#ff4500")
  val orchid = Color.decode( "#da70d6")
  val palegoldenrod = Color.decode( "#eee8aa")
  val palegreen = Color.decode( "#98fb98")
  val paleturquoise = Color.decode( "#afeeee")
  val palevioletred = Color.decode( "#db7093")
  val papayawhip = Color.decode( "#ffefd5")
  val peachpuff = Color.decode( "#ffdab9")
  val peru = Color.decode( "#cd853f")
  val pink = Color.decode( "#ffc0cb")
  val plum = Color.decode( "#dda0dd")
  val powderblue = Color.decode( "#b0e0e6")
  val purple = Color.decode( "#800080")
  val red = Color.decode( "#ff0000")
  val rosybrown = Color.decode( "#bc8f8f")
  val royalblue = Color.decode( "#4169e1")
  val saddlebrown = Color.decode( "#8b4513")
  val salmon = Color.decode( "#fa8072")
  val sandybrown = Color.decode( "#f4a460")
  val seagreen = Color.decode( "#2e8b57")
  val seashell = Color.decode( "#fff5ee")
  val sienna = Color.decode( "#a0522d")
  val silver = Color.decode( "#c0c0c0")
  val skyblue = Color.decode( "#87ceeb")
  val slateblue = Color.decode( "#6a5acd")
  val slategray = Color.decode( "#708090")
  val slategrey = Color.decode( "#708090")
  val snow = Color.decode( "#fffafa")
  val springgreen = Color.decode( "#00ff7f")
  val steelblue = Color.decode( "#4682b4")
  val tan = Color.decode( "#d2b48c")
  val teal = Color.decode( "#008080")
  val thistle = Color.decode( "#d8bfd8")
  val tomato = Color.decode( "#ff6347")
  val turquoise = Color.decode( "#40e0d0")
  val violet = Color.decode( "#ee82ee")
  val wheat = Color.decode( "#f5deb3")
  val white = Color.decode( "#ffffff")
  val whitesmoke = Color.decode( "#f5f5f5")
  val yellow = Color.decode( "#ffff00")
  val yellowgreen = Color.decode( "#9acd32")
  val transparent = new Color(0,0,0,0)


  /** Produces a gradient using the University of Minnesota's school colors, from maroon (low) to gold (high) */
  lazy val MaroonToGold = createGradient(new Color(0xA0, 0x00, 0x00), new Color(0xFF, 0xFF, 0x00), 256)

  /** Produces a gradient from blue (low) to red (high) */
  lazy val BlueToRed= createGradient(Color.BLUE, Color.RED, 500)

  /** Produces a gradient from black (low) to white (high) */
  lazy val BlackToWhite = createGradient(Color.BLACK, Color.WHITE, 500)

  /** Produces a gradient from white (low) to black (high) */
  lazy val WhiteToBlack = createGradient(Color.WHITE, Color.BLACK, 500)

  /** Produces a gradient from red (low) to green (high) */
  lazy val RedToGreen = createGradient(Color.RED, Color.GREEN, 500)

  /** Produces a gradient through green, yellow, orange, red */
  lazy val GreenYelloOrangeRed = createMultiGradient(
    Array(Color.green, Color.yellow, Color.orange, Color.red), 500)

  /** Produces a gradient through the rainbow: violet, blue, green, yellow, orange, red */
  lazy val Rainbow = createMultiGradient(
    Array(new Color(181, 32, 255), Color.blue, Color.green, Color.yellow, Color.orange, Color.red), 500)

  /** Produces a gradient for hot things (black, red, orange, yellow, white) */
  lazy val Hot = createMultiGradient(
    Array(Color.black, new Color(87, 0, 0), Color.red, Color.orange, Color.yellow, Color.white), 500)

  /** Produces a different gradient for hot things (black, brown, orange, white) */
  lazy val Heat = createMultiGradient(
    Array(Color.black, new Color(105, 0, 0), new Color(192, 23, 0), new Color(255, 150, 38), Color.white), 500)

  /** Produces a gradient through red, orange, yellow */
  lazy val RedOrangeYellow = createMultiGradient(
    Array(Color.red, Color.orange, Color.yellow), 500)

  /**
   * Creates an array of Color objects for use as a gradient, using a linear
   * interpolation between the two specified colors.
   *
   * From http://www.mbeckler.org/heatMap/heatMap.html
   *
   * @param one Color used for the bottom of the gradient
   * @param two Color used for the top of the gradient
   * @param numSteps The number of steps in the gradient. 250 is a good number.
   */
  def createGradient(one : Color, two : Color, numSteps : Int) : Array[Color] = {
    val r1 = one.getRed()
    val g1 = one.getGreen()
    val b1 = one.getBlue()

    val r2 = two.getRed()
    val g2 = two.getGreen()
    val b2 = two.getBlue()

    val gradient = new Array[Color](numSteps)
    var iNorm : Double = 0
    for (i <- 0 until numSteps) {
      iNorm = i / numSteps.toDouble; //a normalized [0:1] variable
      val newR = (r1 + iNorm * (r2 - r1)).toInt
      val newG = (g1 + iNorm * (g2 - g1)).toInt
      val newB = (b1 + iNorm * (b2 - b1)).toInt
      gradient(i) = new Color(newR, newG, newB)
    }

    gradient
  }

  /**
   * Creates an array of Color objects for use as a gradient, using an array
   * of Color objects. It uses a linear interpolation between each pair of
   * points.
   *
   * From http://www.mbeckler.org/heatMap/heatMap.html
   *
   * @param colors An array of Color objects used for the gradient. The
   *   Color at index 0 will be the lowest color.
   *
   * @param numSteps The number of steps in the gradient. 250 is a good number.
   */
  def createMultiGradient(colors : Array[Color], numSteps : Int) : Array[Color] = {
    //we assume a linear gradient, with equal spacing between colors
    //The final gradient will be made up of n 'sections', where n = colors.length - 1
    val numSections = colors.length - 1
    var gradientIndex = 0; //points to the next open spot in the final gradient
    val gradient = new Array[Color](numSteps)

    require(numSections > 0, "Array must have at least two colors")

    for (section <- 0 until numSections) {
      //we divide the gradient into (n - 1) sections, and do a regular gradient for each
      val temp = createGradient(colors(section), colors(section+1), numSteps / numSections)
      for (i <- 0 until temp.length) {
        //copy the sub-gradient into the overall gradient
        gradient(gradientIndex) = temp(i)
        gradientIndex += 1
      }
    }

    if (gradientIndex < numSteps) {
      //The rounding didn't work out in our favor, and there is at least
      // one unfilled slot in the gradient[] array.
      //We can just copy the final color there
      while (gradientIndex < numSteps) {
        gradient(gradientIndex) = colors(colors.length - 1)
        gradientIndex += 1
      }
    }

    gradient
  }
}

