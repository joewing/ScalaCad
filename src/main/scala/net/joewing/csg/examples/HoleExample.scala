package net.joewing.csg.examples

import net.joewing.csg._
import net.joewing.csg.primitives._

object HoleExample extends App {
  val obj = cylinder(20, 5, 5, 3) - cylinder(30, 2, 2, 4)
  io.StlAsciiFileWriter.write(obj, "hole.stl")
  io.AwtRenderer.show(obj)
}
