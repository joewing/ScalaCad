package net.joewing.scalacad.examples

import net.joewing.scalacad._
import net.joewing.scalacad.primitives._

object HoleExample extends App {
  val cyl1 = cylinder(10, 3, 3, 16).centered
  val cyl2 = cyl1.rotate(x = math.Pi / 2)
  val cyl3 = cyl1.rotate(y = math.Pi / 2)
  //val obj = (cube(10, 10, 10).centered - (cyl1 | cyl2 | cyl3)) & sphere(6, 16)
  //val obj = cube(10, 10, 10).centered - cylinder(20, 4, 4, 32).centered
  val obj = cube(10, 10, 10) | cube(5, 20, 10)
  io.StlFileWriter.write(obj, "hole.stl")
  io.AwtRenderer.show(obj, showBackfaces = true, showVertices = true)
}
