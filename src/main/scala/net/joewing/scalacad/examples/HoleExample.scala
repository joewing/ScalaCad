package net.joewing.scalacad.examples

import net.joewing.scalacad._
import net.joewing.scalacad.primitives._

object HoleExample extends App {
  val obj = (cube(10, 10, 10).centered - cylinder(10, 3, 3, 16).centered) & sphere(6)
  //val obj = (cube(10, 10, 10).centered - cylinder(10, 3, 3, 5).centered)
  //val obj = cube(10, 10, 10).centered | cube(20, 5, 5).centered
  io.StlAsciiFileWriter.write(obj, "hole.stl")
  io.AwtRenderer.show(io.StlAsciiFileReader.read("hole.stl"), showBackfaces = true, showVertices = true)
}
