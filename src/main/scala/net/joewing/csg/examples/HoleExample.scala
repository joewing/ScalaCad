package net.joewing.csg.examples

import net.joewing.csg._
import net.joewing.csg.primitives._

object HoleExample extends App {
  val obj = Cylinder(20, 5, 5, 3) - Cylinder(30, 2, 2, 4)
  val stl = Stl("Hole", obj.render.allFacets)
  io.StlAsciiFileWriter.write(stl, "hole.stl")
  io.AwtRenderer.show(stl)
}
