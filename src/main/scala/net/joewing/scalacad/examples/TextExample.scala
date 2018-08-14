package net.joewing.scalacad.examples

import net.joewing.scalacad.io.{AwtRenderer, StlFileWriter}
import net.joewing.scalacad.primitives._

object TextExample extends App {

  val text = "Hello!"
  val textDepth = 5
  val textObj = Raster.fromText(text, 64).extrude(textDepth).centered

  val padding = 16
  val boxWidth = textObj.extent.x + padding
  val boxHeight = textObj.extent.y + padding
  val boxDepth = boxHeight
  val box = Cube(boxWidth, boxHeight, boxDepth).centered

  val obj = textObj.above(box, overlap = textDepth, Difference.apply).rotate(x = math.Pi / 4) -
    cube(boxWidth * 2, boxHeight * 2, boxDepth).centered.translate(z = -boxHeight / 2)
  AwtRenderer.show(obj)
}
