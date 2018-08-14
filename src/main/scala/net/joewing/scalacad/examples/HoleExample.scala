package net.joewing.scalacad.examples

import net.joewing.scalacad._
import net.joewing.scalacad.primitives._

object HoleExample extends App {
  val obj = (cube(10, 10, 10).centered - cylinder(10, 3, 3, 16).centered) & sphere(6)
  io.AwtRenderer.show(obj)
}
