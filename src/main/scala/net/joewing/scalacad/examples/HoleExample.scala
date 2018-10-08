package net.joewing.scalacad.examples

import net.joewing.scalacad._
import net.joewing.scalacad.primitives._

object HoleExample extends App {
  val cyl1 = cylinder(12, 3).centered
  val cyl2 = cyl1.rotate(x = math.Pi / 2)
  val cyl3 = cyl1.rotate(y = math.Pi / 2)
  val obj = sphere(6) - (cyl1 | cyl2 | cyl3)
  io.StlFileWriter.write(obj, "hole.stl")
  io.AwtRenderer.show(obj)
}
