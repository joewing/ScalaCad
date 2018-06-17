package net.joewing.csg.examples

import net.joewing.csg.io.AwtRenderer
import net.joewing.csg.primitives._

object ScrewExample extends App {
  val turnsPerMM = 2.0
  val length = 1.0
  val resolution = 0.2
  val slicesPerMM = 1.0 / resolution
  val slices = (length * slicesPerMM).toInt
  val radiansPerSlice = math.Pi * 2.0 * turnsPerMM / slicesPerMM
  val obj = circle(10, 8).scale(x = 0.5).centered.extrude(length, radiansPerSlice, slices)
  AwtRenderer.show(obj)
}
