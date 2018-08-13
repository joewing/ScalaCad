package net.joewing.scalacad.examples

import net.joewing.scalacad.io.{AwtRenderer, StlFileWriter}
import net.joewing.scalacad.primitives._

object TextExample extends App {

  val text = "Hello!"
  val textDepth = 8
  val textObj = Raster.fromText(text, 64).extrude(textDepth).centered

  val padding = 16
  val boxWidth = textObj.extent.x + padding
  val boxHeight = textObj.extent.y + padding
  val boxDepth = boxHeight
  val box = Cube(boxWidth, boxHeight, boxDepth).centered

  val obj = textObj.above(box, overlap = textDepth, Difference.apply)
  StlFileWriter.write(obj, "text.stl")
  AwtRenderer.show(obj)
}
