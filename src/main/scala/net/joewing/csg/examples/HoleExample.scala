package net.joewing.csg.examples

import net.joewing.csg._
import net.joewing.csg.primitives._

object HoleExample extends App {
  val obj = (cube(10, 10, 10).centered - cylinder(10, 3, 3, 16).centered) & sphere(6)
  val stl = Stl("hole", obj.render.facets)
  io.StlAsciiFileWriter.write(stl, "hole.stl")
  io.AwtRenderer.show(obj)
}
