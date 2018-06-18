package net.joewing.csg.examples

import net.joewing.csg.io.AwtRenderer
import net.joewing.csg.primitives._

object ScrewExample extends App {
  val turns = 1
  val length = 1.0
  val resolution = 0.2
  val circleSides = 5

  val slices = (length / resolution).toInt
  val radiansPerSlice = math.Pi * 2.0 * turns / slices
  val obj = circle(10, circleSides).scale(x = 0.75).extrude(length, radiansPerSlice, slices)
  AwtRenderer.show(obj)
}
