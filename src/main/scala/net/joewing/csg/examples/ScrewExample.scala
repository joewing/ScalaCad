package net.joewing.csg.examples

import net.joewing.csg.io.{AwtRenderer, StlAsciiFileWriter}
import net.joewing.csg.primitives._

object ScrewExample extends App {
  val turns = 2
  val length = 8.0
  val resolution = 0.2
  val circleSides = 16

  val slices = (length / resolution).toInt
  val radiansPerSlice = math.Pi * 2.0 * turns / slices
  val obj = circle(10, circleSides).scale(x = 0.75).extrude(length, radiansPerSlice, slices)
  StlAsciiFileWriter.write(obj, "screw.stl")
  AwtRenderer.show(obj)
}
