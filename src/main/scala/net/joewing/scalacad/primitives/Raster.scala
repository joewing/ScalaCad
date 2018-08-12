package net.joewing.scalacad.primitives

import java.awt.{Color, Font}
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.{FileInputStream, InputStream}

import javax.imageio.ImageIO
import net.joewing.scalacad.{Facet, Surface}

case class Raster(grid: Vector[Vector[Boolean]], resolution: Double) extends Primitive[TwoDimensional] {

  def pixel(x: Int, y: Int): Seq[Facet] = {
    val rect = Rectangle(resolution, resolution).render.facets
    rect.map(_.moved(x * resolution, y * resolution))
  }

  def render: Surface = {
    val width = grid.head.length
    val height = grid.length
    Surface.fromFacets {
      (0 until height).flatMap { y =>
        (0 until width).flatMap { x =>
          if (grid(height - y - 1)(x)) pixel(x, y) else Vector.empty
        }
      }
    }
  }
}

object Raster {
  def fromImage(
    image: BufferedImage,
    resolution: Double = 0.2,
    threshold: Double = 0.5,
    invert: Boolean = false
  ): Raster = {
    val width = image.getWidth
    val height = image.getHeight
    val thresholdSum = 3 * 255 * threshold
    val grid = Vector.tabulate[Boolean](height, width) { (y, x) =>
      val rgb = image.getRGB(x, y)
      val red = (rgb >> 16) & 0xFF
      val green = (rgb >> 8) & 0xFF
      val blue = rgb & 0xFF
      val total = red + green + blue
      if (invert) total < thresholdSum else total >= thresholdSum
    }
    Raster(grid, resolution)
  }

  def fromStream(
    is: InputStream,
    resolution: Double = 0.2,
    threshold: Double = 0.5,
    invert: Boolean = false
  ): Raster = {
    fromImage(ImageIO.read(is), resolution, threshold, invert)
  }

  def fromFile(
    fileName: String,
    resolution: Double = 0.2,
    threshold: Double = 0.5,
    invert: Boolean = false
  ): Raster = {
    fromStream(new FileInputStream(fileName), resolution, threshold, invert)
  }

  def fromText(
    text: String,
    size: Double = 12.0,
    font: String = Font.DIALOG,
    resolution: Double = 0.2
  ): Raster = {
    val transform = new AffineTransform()
    val frc = new FontRenderContext(transform, true, true)
    val f = Font.decode(font).deriveFont(size.toFloat)
    val bounds = f.getStringBounds(text, frc)
    val width = bounds.getWidth.toInt
    val height = bounds.getHeight.toInt
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val gc = image.getGraphics
    gc.setFont(f)
    gc.setColor(Color.BLACK)
    gc.fillRect(0, 0, width, height)
    gc.setColor(Color.WHITE)
    gc.drawString(text, 0, -bounds.getY.toInt)
    fromImage(image, resolution)
  }
}
