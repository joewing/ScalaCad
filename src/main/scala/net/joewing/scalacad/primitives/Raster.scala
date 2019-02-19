package net.joewing.scalacad.primitives

import java.awt.{Color, Font}
import java.awt.font.FontRenderContext
import java.awt.geom.AffineTransform
import java.awt.image.BufferedImage
import java.io.{FileInputStream, InputStream}

import javax.imageio.ImageIO

object Raster {

  def apply(grid: Vector[Vector[Boolean]], resolution: Double): Primitive2d = {
    val rect = Rectangle(resolution, resolution).rendered.polygons
    val width = grid.head.length
    val height = grid.length
    val polygons = {
      (0 until height).flatMap { y =>
        (0 until width).flatMap { x =>
          if (grid(height - y - 1)(x)) {
            rect.map(_.moved(x * resolution, y * resolution))
          } else Vector.empty
        }
      }
    }
    Primitive2d(polygons)
  }

  def fromImage(
    image: BufferedImage,
    resolution: Double = 0.2,
    threshold: Double = 0.5,
    invert: Boolean = false
  ): Primitive2d = {
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
  ): Primitive2d = {
    fromImage(ImageIO.read(is), resolution, threshold, invert)
  }

  def fromFile(
    fileName: String,
    resolution: Double = 0.2,
    threshold: Double = 0.5,
    invert: Boolean = false
  ): Primitive2d = {
    fromStream(new FileInputStream(fileName), resolution, threshold, invert)
  }

  def fromText(
    text: String,
    font: Font = Font.decode(Font.DIALOG),
    resolution: Double = 0.2
  ): Primitive2d = {
    val transform = new AffineTransform()
    val frc = new FontRenderContext(transform, true, true)
    val bounds = font.getStringBounds(text, frc)
    val width = bounds.getWidth.ceil.toInt * 2
    val height = bounds.getHeight.ceil.toInt
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val gc = image.getGraphics
    gc.setFont(font)
    gc.setColor(Color.BLACK)
    gc.fillRect(0, 0, width, height)
    gc.setColor(Color.WHITE)
    gc.drawString(text, -bounds.getX.toInt, -bounds.getY.toInt)
    fromImage(image, resolution)
  }
}
