package breeze.plot


import java.io.{File,OutputStream,FileOutputStream,IOException}
import java.awt.Graphics2D

/**
 * Utility functions for exporting a Graphics2D drawable to some eps, pdf,
 * and png.
 *
 * @author dramage, Robby McKilliam
 */
object ExportGraphics {
  /** A Drawable is any function that draws to a Graphics2D context. */
  type Drawable = ((Graphics2D)=>Unit)

  /**
   * Writes the given drawable to a new file of the given name with
   * the given dpi (for rasterized formats only).  The extension of the file
   * determines its format, with options png, eps, svg, and pdf.
   */
  def writeFile(file : File, draw : Drawable, width : Int, height : Int, dpi : Int = 72) = {
    lazy val fos = new FileOutputStream(file)
    if (file.getName.toLowerCase.endsWith(".png")) {
      try {
        writePNG(fos,draw,width,height,dpi)
      } finally {
        fos.close()
      }
    } else if (file.getName.toLowerCase.endsWith(".eps")) {
      try {
        writeEPS(fos,draw,width,height)
      } finally {
        fos.close()
      }
    } else if (file.getName.toLowerCase.endsWith(".pdf")) {
      try {
        writePDF(fos,draw,width,height)
      } finally {
        fos.close()
      }
//    } else if (file.getName.toLowerCase.endsWith(".svg")) {
//      try {
//        writeSVG(fos,draw,width,height)
//      } finally {
//        fos.close()
//      }
    } else {
      throw new IOException("Unrecognized file extension: should be png, svg, eps, or pdf")
    }
  }

  /**
   * Writes the given drawable to the given OutputStream at the given dpi,
   * formatted as png.
   */
  def writePNG(out : OutputStream, draw : Drawable, width : Int, height : Int, dpi : Int = 72) {
    import javax.imageio.ImageIO
    import java.awt.image.BufferedImage

    // default dpi is 72
    val scale = dpi / 72.0
    val swidth = (width * scale).toInt
    val sheight = (height * scale).toInt

    val image = new BufferedImage(swidth,sheight,BufferedImage.TYPE_INT_ARGB)
    val g2d = image.createGraphics()
    g2d.scale(scale, scale)
    draw(g2d)
    g2d.dispose

    ImageIO.write(image, "png", out)
  }

  /**
   * Writes the given drawable to the given OutputStream formatted as eps.
   */
  def writeEPS(out : OutputStream, draw : Drawable, width : Int, height : Int) {
    import org.apache.xmlgraphics.java2d.ps.EPSDocumentGraphics2D
    import org.apache.xmlgraphics.java2d.GraphicContext

    val g2d = new EPSDocumentGraphics2D(false)
    g2d.setGraphicContext(new GraphicContext)
    g2d.setupDocument(out, width, height)
    draw(g2d)
    g2d.finish()
  }

  /**
   * Writes the given drawable to the given OutputStream formatted as pdf.
   * Contributed by Robby McKilliam.
   */
  def writePDF(out : OutputStream, draw : Drawable, width : Int, height : Int) {
    import com.lowagie.text.Document
    import com.lowagie.text.Rectangle
    import com.lowagie.text.pdf.PdfWriter

    val document = new Document()

    try {
      document.setPageSize(new Rectangle(width, height))
      val writer = PdfWriter.getInstance(document, out)
      document.open()

      val cb = writer.getDirectContent
      val tp = cb.createTemplate(width, height)
      val g2d = tp.createGraphics(width, height)

      draw(g2d)

      g2d.dispose()

      cb.addTemplate(tp, 1, 0, 0, 1, 0, 0)
    } finally {
      document.close()
    }
  }

//  /**
//   * Writes the given drawable to the given OutputStream formatted as svg.
//   */
//  def writeSVG(out : OutputStream, draw : Drawable, width : Int, height : Int) {
//    import org.apache.batik.svggen.SVGGraphics2D
//    import org.apache.batik.dom.GenericDOMImplementation
//    import org.w3c.dom.Document
//    import java.io.OutputStreamWriter
//
//    val dom = GenericDOMImplementation.getDOMImplementation()
//    val document = dom.createDocument("http://www.w3.org/2000/svg","svg",null)
//    val g2d = new SVGGraphics2D(document)
//    draw(g2d)
//    g2d.stream(new OutputStreamWriter(out, "UTF-8"), true)
//    g2d.dispose()
//  }
}

