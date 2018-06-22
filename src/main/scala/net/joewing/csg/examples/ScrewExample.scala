package net.joewing.csg.examples

import net.joewing.csg.io.{AwtRenderer, StlAsciiFileWriter}
import net.joewing.csg.primitives._

object ScrewExample extends App {

  val resolution = 0.2

  val screwTurns = 2.0
  val screwLength = 8.0
  val screwSides = 20
  val screwRadius = 4.0
  val innerRadius = 3.0

  val nutLength = 4.0
  val nutRadius = 10.0
  val nutSides = 6

  val nutTurns = screwTurns * nutLength / screwLength

  def threads(length: Double, turns: Double): Primitive[ThreeDimensional] = {
    val slices = (length / resolution).toInt
    val radiansPerSlice = math.Pi * 2.0 * turns / slices
    circle(screwRadius, screwSides).scale(x = innerRadius / screwRadius).extrude(length, radiansPerSlice, slices)
  }

  def cap: Primitive[ThreeDimensional] = {
    cylinder(nutLength, nutRadius, nutRadius, nutSides)
  }

  def screwThreads = threads(screwLength, screwTurns)

  def screw: Primitive[ThreeDimensional] = {
    cap | screwThreads.translate(z = nutLength)
  }

  def nutThreads = threads(nutLength, nutTurns)

  def nut: Primitive[ThreeDimensional] = {
    cap - nutThreads
  }

  val obj = screw.beside(nut, overlap = -nutRadius)
  StlAsciiFileWriter.write(obj, "screw.stl")
  AwtRenderer.show(obj)
}
