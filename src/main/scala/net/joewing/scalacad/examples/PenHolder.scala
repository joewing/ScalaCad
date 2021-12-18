package net.joewing.scalacad.examples

import net.joewing.scalacad.primitives._
import net.joewing.scalacad.io._

object PenHolder extends App {

  val tol = 0.4 * 3

  val logoFile = "/Users/joewing/wp-logo-support.png"
  val logoScale = 0.9
  val logo = Raster.fromFile(logoFile, invert = true).extrude(10).centered
    .scale(x = logoScale, y = logoScale)
    .rotate(z = math.Pi)
  val sides = cylinder(100, 50, 50, 6) - cylinder(100, 50 - tol, 50 - tol, 6)
  val top = (sides - disjointUnion[ThreeDimensional](
    Range(0, 6).map { i =>
      val theta = i * 2 * math.Pi / 6 + math.Pi / 6
      val x = 45 * math.cos(theta)
      val y = 45 * math.sin(theta)
      logo
        .rotate(y = math.Pi / 2)
        .rotate(x = math.Pi / 2)
        .rotate(z = theta)
        .translate(x = x, y = y)
        .translate(z = 50 - logo.extent.y / 2)
    }: _*
  )).rotate(y = math.Pi)
  val bottom = cylinder(10, 50, 50, 6).translate(z = 100).rotate(y = math.Pi)

  StlFileWriter.write(top, "penholder-top.stl")
  StlFileWriter.write(bottom, "penholder-bottom.stl")

  AwtRenderer.show(top | bottom)

}
