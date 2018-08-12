package net.joewing.scalacad.primitives

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
          if (grid(y)(x)) pixel(x, y) else Vector.empty
        }
      }
    }
  }
}

object Raster {
  def fromImage(image: BufferedImage, resolution: Double = 0.2): Raster = {
    val width = image.getWidth
    val height = image.getHeight
    val grid = Vector.tabulate[Boolean](height, width) { (y, x) =>
      val rgb = image.getRGB(x, y)
      val red = (rgb >> 16) & 0xFF
      val green = (rgb >> 8) & 0xFF
      val blue = rgb & 0xFF
      (red + green + blue) / 3.0 >= 250
    }
    Raster(grid, resolution)
  }

  def fromStream(is: InputStream, resolution: Double = 0.2): Raster = {
    fromImage(ImageIO.read(is), resolution)
  }

  def fromFile(fileName: String, resolution: Double = 0.2): Raster = {
    fromStream(new FileInputStream(fileName), resolution)
  }
}
